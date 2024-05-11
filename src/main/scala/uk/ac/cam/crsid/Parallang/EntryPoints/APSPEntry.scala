package uk.ac.cam.crsid.Parallang.EntryPoints

import uk.ac.cam.crsid.Parallang.Interpreter.CommunicationModel.{Datacenter, HighPowerInternet, InterconnectionNetwork, MulticoreComputer}
import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.Listeners.{OptimizedTimeTallyListener, ReceiveTriggeredTallyListener, TimeTallyListener}
import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.TimingLong._
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.ReplacementStrategy.{LeastRecentlyUsed, NotLastUsed}
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.{CacheHierarchy, DirectMappedCache, SetAssociativeCache, TreePLRUSetAssociativeCache}
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Listeners.{CurrentMemoryListener, PeakMemoryListener, SampleBasedMemoryListener}
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.{ArrayValue, IntegerValue}
import uk.ac.cam.crsid.Parallang.TypeChecking.{ArrayType, IntType}
import uk.ac.cam.crsid.lib.Events.Listeners.TallyListener
import uk.ac.cam.crsid.lib.FileHandling.{EdgesReader, MatrixDumper}

import java.lang.Integer.parseInt
import java.nio.file.Paths
//import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.sys.exit

object APSPEntry {

  def main(args: Array[String]): Unit = {
    /*
     * Args:
     *   q: int
     *   pathToProgram: string
     *   pathToInputMatrix: string
     *   dumpDir: string
     *   latencyParameterSet: string
     */
    if (args.length != 6) {
      println("Usage: ./apsp_entry <q> <pathToProgram> <pathToInput> <dumpDir> <latencyParameterSet=MulticoreComputer|Datacenter> <num samples>")
      exit()
    }
    val q: Int = parseInt(args(0))
    val pathToProgram = args(1)
    val pathToInput = args(2)
    val dumpDir = args(3)
    val parameterSet = args(4)
    val numSamples = parseInt(args(5))
    val (numNodes, edgeList) = EdgesReader.readEdges(pathToInput)

    println(s"Program: $pathToProgram")
    println(s"Input: $pathToInput")

    val cacheHierarchy = new CacheHierarchy(2, 51 nanoseconds)
      // 8-way 32K cache, 4x16 word cache blocks, assume latency absorbed by pipeline
      .setLevel(1, new DirectMappedCache(32768/(16*4)/8, 4, 0))
      // 4-way 256K cache, 4x16 word cache blocks, 12 cycles * 454.545454 picosceonds per cycle 5454.54... = 5455 ps
      .setLevel(2, new DirectMappedCache(262144/(16*4)/8, 4, 5455))
      // 12-way 9M cache, 4x16 word cache blocks
//      .setLevel(3, new SetAssociativeCache(9437184/(16*4), 4, 0.nanoseconds, 12, new LeastRecentlyUsed))

    val latencyParameterSet = if (parameterSet == "MulticoreComputer") {
      println("Parameter Set: Multicore Computer")
      MulticoreComputer
    } else if (parameterSet == "Datacenter") {
      println("Parameter Set: Datacenter")
      Datacenter
    } else if (parameterSet == "HighPowerInternet") {
      println("Parameter Set: HighPowerInternet")
      HighPowerInternet
    } else {
      println(s"Invalid parameter set: $parameterSet")
      exit()
    }

    println(s"Processor grid: $q x $q")
    InterconnectionNetwork.resetLengthWidth(q, q, cacheHierarchy=cacheHierarchy, numSamples=numSamples)
    InterconnectionNetwork.setLatencyParameterSet(latencyParameterSet)
    InterconnectionNetwork.torus()
//    InterconnectionNetwork.addGlobalLatencyListener(new TallyListener)
    InterconnectionNetwork.addGlobalLatencyListener(new OptimizedTimeTallyListener)

//    InterconnectionNetwork.addGlobalMemoryListener(new TallyListener)
    InterconnectionNetwork.addGlobalMemoryListener(new SampleBasedMemoryListener(50 milliseconds))
    InterconnectionNetwork.addGlobalMemoryListener(new PeakMemoryListener)
//    InterconnectionNetwork.addGlobalMemoryListener(new CurrentMemoryListener) // canary in a coal mine

    InterconnectionNetwork.broadcastIntAsExternal(q)

    InterconnectionNetwork.sendArrayAsExternal((0, 0), edgeList.asInstanceOf[Array[Any]])
    InterconnectionNetwork.sendIntAsExternal((0, 0), numNodes)

    println("Launching...")
    InterconnectionNetwork.launchWith(pathToProgram)
    InterconnectionNetwork.printSummaryDiagnostics()
    InterconnectionNetwork.dumpSummaryDiagnostics(dumpDir)

    val C = InterconnectionNetwork.recvAsExternal((0, 0), ArrayType(ArrayType(IntType())))
    val P = InterconnectionNetwork.recvAsExternal((0, 0), ArrayType(ArrayType(IntType())))

    val Pmatr = ArrayValue.matrixToScalaArray(P.v.asInstanceOf[ArrayValue])
    val Cmatr = ArrayValue.matrixToScalaArray(C.v.asInstanceOf[ArrayValue])

    MatrixDumper.dump(Paths.get(dumpDir, "predecessor.txt").toString, Pmatr)
    MatrixDumper.dump(Paths.get(dumpDir, "distances.txt").toString, Cmatr)
  }

}
