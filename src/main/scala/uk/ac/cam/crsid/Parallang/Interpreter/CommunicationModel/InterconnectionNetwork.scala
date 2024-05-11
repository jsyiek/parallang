package uk.ac.cam.crsid.Parallang.Interpreter.CommunicationModel

import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.LatencyEvent
import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.LatencyEvent.numSamples
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.CacheHierarchy
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.MemoryEvent
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.{ArrayValue, IntegerValue, UnitValue, Value}
import uk.ac.cam.crsid.Parallang.Interpreter.ProcessingElement
import uk.ac.cam.crsid.Parallang.ParallangFrontEnd
import uk.ac.cam.crsid.Parallang.TypeChecking.{IntType, Type}
import uk.ac.cam.crsid.lib.Events.EventListener
import uk.ac.cam.crsid.lib.Exception.InvalidRouteException

import java.io.{File, InputStream, PrintWriter}
import java.lang.Thread.currentThread
import java.nio.file.Paths
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, Future}

object InterconnectionNetwork {

  private var fastestRoutes: Array[Array[Long]] = Array()
  private var topology: Array[Array[ProcessingElement]] = Array()
  private var rows = 0
  private var cols = 0

  private var running = false

  private var queueForExternal: Array[StrictFIFOQueue] = Array()
  private var externalSeqNumbs: Array[Int] = Array()

  private val myThread: Thread = currentThread()

  private var fatalThreadError: Option[Throwable] = None

  private var latencyParameterSet: LatencyParameterSet = MulticoreComputer

  def resetLengthWidth(rows: Int, cols: Int, cacheHierarchy: CacheHierarchy = new CacheHierarchy(0, 100L /* ps */), doPrint: Boolean = true, numSamples: Int = 1): Unit = {
    LatencyEvent.numSamples = numSamples
    fastestRoutes = Array.fill(rows*cols) { Array.fill(rows*cols) {  Long.MaxValue } }
    0 until rows*cols foreach {
      i => fastestRoutes(i)(i) = 0
    }
    topology = Array.tabulate(rows) { r => Array.tabulate(cols) { c => new ProcessingElement((r, c), rows, cols, cacheHierarchy.duplicate(), doPrint) } }
    this.rows = rows
    this.cols = cols
    queueForExternal = Array.fill(rows*cols) { new StrictFIFOQueue() }
    externalSeqNumbs = Array.fill(rows*cols) { 0 }

    fatalThreadError = None
  }

  def setLatencyParameterSet(params: LatencyParameterSet): Unit = latencyParameterSet = params
  def getLatencyParameterSet: LatencyParameterSet = latencyParameterSet

  /**
   * Takes a row and a column (in THAT ORDER) and maps it to an index
   *
   * @param coord: A pair of (row, col)
   * @return index of the array
   */
  def index: (Int, Int) => Int = _*cols+_

  /**
   * Idempotently adds a bidirectional link between two processors
   * @param pe1: First processor (row, col)
   * @param pe2: Second processor (row, col)
   */
  def addLink(pe1: (Int, Int), pe2: (Int, Int)): Unit = (pe1, pe2) match {
    case ((r1, c1), (r2, c2)) =>
      fastestRoutes(index(r1, c1))(index(r2, c2)) = 1
      fastestRoutes(index(r2, c2))(index(r1, c1)) = 1
  }

  /**
   * A shortcut to automatically set up a torus topology.
   */
  def torus(): Unit = {
    for {
      i <- 0 until rows
      j <- 0 until cols
    } {
      addLink((i, j), ((i+1) % rows, j))
      addLink((i, j), ((i+rows-1) % rows, j))
      addLink((i, j), (i, (j+1) % cols))
      addLink((i, j), (i, (j+cols-1) % cols))
    }
  }

  /**
   * Invokes Floyd-Warshall's algorithm to generate the smallest length paths between nodes
   * for the event of messages requiring forwarding
   */
  private def computeFastestRoutes(): Unit = {
    // A simple sequential implementation of Floyd-Warshall's algorithm
    for {
      i <- 0 until rows*cols
      j <- 0 until rows*cols
      k <- 0 until rows*cols
    } {
      if (fastestRoutes(i)(k) == Long.MaxValue || fastestRoutes(k)(j) == Long.MaxValue) {}
      else if (fastestRoutes(i)(j) > fastestRoutes(i)(k) + fastestRoutes(k)(j)) {
        fastestRoutes(i)(j) = fastestRoutes(i)(k) + fastestRoutes(k)(j)
      }
    }
  }

//:)

  def sendDirect(sender: (Int, Int),
                 other: (Int, Int),
                 message: Value,
                 typed: Type,
                 avgSendTs: Long,
                 allSendTs: Array[Long],
                 seqNum: Int,
                 payCachePenalty: Boolean = true): Unit = {
    val copiedMessage = message.deepCopy()
//    if (payCachePenalty) penalizeSenderForSending(topology(sender._1)(sender._2), message)

    val otherPE = topology(other._1)(other._2)
    val senderIndex = index(sender._1, sender._2)
    val receiverIndex = index(other._1, other._2)
    val fastestRoute = fastestRoutes(senderIndex)(receiverIndex)

    if (fastestRoute == Long.MaxValue) {
      throw new InvalidRouteException(s"No forwarding path exists connecting $sender and $other")
    }

    Future {
      otherPE.messageQueue(senderIndex).giveMessage(
        Message(
          copiedMessage,
          typed,
          avgSendTs + latencyParameterSet.getWireLatencyTime(fastestRoutes(senderIndex)(receiverIndex)),
          allSendTs map (_ + latencyParameterSet.getWireLatencyTime(fastestRoutes(senderIndex)(receiverIndex))),
          seqNum)
      )
    }
  }

  // Assumes the value passed is a deep copy.
  def broadcast(sender: (Int, Int),
                message: Value,
                typed: Type,
                avgSendTs: Long,
                allSendTs: Array[Long],
                seqNumbs: IndexedSeq[Int]): Unit = {
    val copiedMessage = message.deepCopy()
//    penalizeSenderForSending(topology(sender._1)(sender._2), message, copiedMessage)
    Future {
      for {
        r <- 0 until rows
        c <- 0 until cols
      } {
        if ((r, c) != sender) {
          // the message will be deep copied again by each future spawned here
          sendDirect(sender, (r, c), copiedMessage, typed, avgSendTs, allSendTs, seqNumbs(InterconnectionNetwork.index(r, c)), payCachePenalty = false)
        }
      }
    }
  }

  def broadcastRow(sender: (Int, Int),
                   message: Value,
                   typed: Type,
                   avgSendTs: Long,
                   allSendTs: Array[Long],
                   seqNumbs: IndexedSeq[Int]): Unit = {
    // even if broadcasting, we only need to penalize once
    val copiedMessage = message.deepCopy()
//    penalizeSenderForSending(topology(sender._1)(sender._2), message, copiedMessage)
    val (sRow, sCol) = sender
    Future {
      for {
        c <- 0 until cols
      }
        if (sCol != c) {
          sendDirect(sender, (sRow, c), copiedMessage, typed, avgSendTs, allSendTs, seqNumbs(c), payCachePenalty = false)
        }
    }
  }

  def broadcastCol(sender: (Int, Int),
                   message: Value,
                   typed: Type,
                   avgSendTs: Long,
                   allSendTs: Array[Long],
                   seqNumbs: IndexedSeq[Int]): Unit = {
    // even if broadcasting, we only need to penalize once
    val copiedMessage = message.deepCopy()
//    penalizeSenderForSending(topology(sender._1)(sender._2), message, copiedMessage)
    val (sRow, sCol) = sender
    Future {
      for {
        r <- 0 until rows
      }
        if (sRow != r) {
          sendDirect(sender, (r, sCol), copiedMessage, typed, avgSendTs, allSendTs, seqNumbs(r), payCachePenalty = false)
        }
    }
  }

  def broadcastIntAsExternal(message: Int, sendTs: Long = 0): Unit = {
    for {
      r <- 0 until rows
      c <- 0 until cols
    } {
      sendIntAsExternal((r, c), message, sendTs)
    }
  }

  def sendIntAsExternal(target: (Int, Int), message: Long, sendTs: Long = 0): Unit = {
    val (tRow, tCol) = target
    val targetIndex = InterconnectionNetwork.index(tRow, tCol)
    topology(tRow)(tCol).messageQueue(rows*cols).giveMessage(
      Message(new IntegerValue(message), IntType(), sendTs, Array.fill(numSamples)(sendTs), externalSeqNumbs(targetIndex))
    )
    externalSeqNumbs(targetIndex) += 1
  }

  def sendArrayAsExternal(target: (Int, Int), message: Array[Any], sendTs: Long = 0): Unit = {
    val (tRow, tCol) = target
    val targetIndex = InterconnectionNetwork.index(tRow, tCol)
    val (arrayVal, typed) = ArrayValue.fromScalaArray(message)
    val q = topology(tRow)(tCol).messageQueue(rows*cols)
    q.giveMessage(Message(arrayVal, typed, sendTs, Array.fill(numSamples)(sendTs), externalSeqNumbs(targetIndex)))
    externalSeqNumbs(targetIndex) += 1
  }

  def sendToExternal(sender: (Int, Int), message: Message): Unit = {
    Future {
      val (sRow, sCol) = sender
      val sIndex = index(sRow, sCol)
      queueForExternal(sIndex).giveMessage(message)
    }
  }

  def recvAsExternal(from: (Int, Int), t: Type): Message = queueForExternal(index(from._1, from._2)).getFirst(t)

  def launchWith(programFilepath: String): Unit = {
    launchWith(ParallangFrontEnd.filenameToInputStream(programFilepath))
  }

  def launchWith(programStream: InputStream, throws: Boolean = true): Unit = {
    val (fileContents, stream) = ParallangFrontEnd.readFileFromIOStream(programStream)
    val ast = try {
      ParallangFrontEnd.parseAndTypeCheck(stream)
    } catch {
      case e: Throwable =>
        if (throws)
          throw e
        else {
          ParallangFrontEnd.printError(e, fileContents)
          return
        }
    }
    computeFastestRoutes()
    var threads: List[Thread] = List()
    for {
      i <- 0 until rows
      j <- 0 until cols
    } {
      val processorThread = new Thread {
        override def run(): Unit = {
          topology(i)(j).executeStart(ast)
        }
      }
      threads = processorThread :: threads
      processorThread.start()
    }
    running = true
    try {
      threads foreach (_.join())
    } catch {
      case _: InterruptedException =>
        threads foreach (_.interrupt())
        fatalThreadError = topology.flatten find (_.whyFailed.nonEmpty) flatMap (_.whyFailed)
    }
    running = false
    fatalThreadError map { throw _ }
  }

  def addGlobalLatencyListener(listener: => EventListener[LatencyEvent]): Unit = {
    for {
      i <- 0 until rows
      j <- 0 until cols
    } {
      topology(i)(j).latencyContext.subscribe(listener)
    }
  }

  def addGlobalMemoryListener(listener: => EventListener[MemoryEvent]): Unit = {
    for {
      i <- 0 until rows
      j <- 0 until cols
    } {
      topology(i)(j).executionContext.subscribe(listener)
    }
  }

  def tickDuringNextRun(tick: Int = 1000): Unit = {
    Future {
      var wasRunning = false
      var start = System.currentTimeMillis()
      while (!wasRunning || running) {
        if (!wasRunning && running) {
          start = System.currentTimeMillis()
        }
        if (running) {
          println(s"Tick: ${System.currentTimeMillis() - start}")
        }
        wasRunning = running
        Thread.sleep(tick)
      }
    }
  }

  def interruptExecution(): Unit = this.synchronized {
    myThread.interrupt()
    // There's a nasty race condition that can occur that this prevents.
    // After signalling the coordinator that the run needs to be aborted,
    // if all threads have terminated before the interrupt is delivered
    // (as the interrupt is delivered asynchronously to `myThread.interrupt()`'s
    // return) then the coordinator will move to exit normally.
    // To ensure we do not let all threads terminate, the interrupter must be
    // held captive until interrupted by the coordinator.
    while (true) {
      Thread.`yield`()
    }
  }

  def printSummaryDiagnostics(): Unit = {
    println("================== LATENCY ==================")
    for {
      i <- 0 until rows
      j <- 0 until cols
    } {
      val pe = topology(i)(j)
      println(s"PE ${pe.whoAmI}: \n\t${pe.latencyContext} \n\t${pe.executionContext}")
    }
    println("=============================================")
  }

  def dumpSummaryDiagnostics(dumpDir: String): Unit = {
    val dumpFileName = s"${topology.length}x${topology(0).length}_${latencyParameterSet.getClass.getSimpleName}_${System.currentTimeMillis()}.txt"
    val dumpPath = Paths.get(dumpDir, dumpFileName).toString
    val file = new File(dumpPath)
    file.createNewFile()
    val pw = new PrintWriter(file)
    pw.write("==================  SUMMARY  ==================\n")
    for {
      i <- 0 until rows
      j <- 0 until cols
    } {
      val pe = topology(i)(j)
      pw.write(s"PE ${pe.whoAmI}: \n\t${pe.latencyContext} \n\t${pe.executionContext}\n")
    }
    pw.write("===============================================\n")
    pw.close()
  }

}
