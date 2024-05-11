package uk.ac.cam.crsid.Parallang

import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.{Cache, CacheHierarchy}
import uk.ac.cam.crsid.Parallang.Interpreter.ProcessingElement
import uk.ac.cam.crsid.Parallang.Parser.{ASTStart, ParallogParser, ParseException, SimpleNode}
import uk.ac.cam.crsid.Parallang.TypeChecking.TypeChecker
import uk.ac.cam.crsid.lib.Exception.{BlameTokenException, NameDoesNotExistException}

import java.io.{BufferedInputStream, ByteArrayInputStream, File, FileInputStream, InputStream}
import java.nio.charset.StandardCharsets
import java.util.Scanner
import scala.concurrent.duration.DurationInt
import scala.sys.exit
import scala.util.{Failure, Success, Try}

object ParallangFrontEnd {

  def filenameToInputStream(filepath: String): java.io.InputStream = {
    val initialFile: File = new File(filepath);
    new FileInputStream(initialFile)
  }

  def generateSyntaxTree(ioStream: java.io.InputStream): Try[ASTStart] = {
    val parser = new ParallogParser(ioStream)
    val startNode = try {
      parser.Start()
    } catch {
      case e: ParseException => return Failure(e)
    }
    Success(startNode);
  }

  def typeCheckSyntaxTree(startNode: ASTStart): Try[ASTStart] = TypeChecker.evaluate(startNode).map(_ => startNode)

  def readFileFromIOStream(targetStream: java.io.InputStream): (String, InputStream) = {
    val s = new Scanner(targetStream).useDelimiter("\\A")
    val result = if (s.hasNext) s.next else ""
    val stream: InputStream = new ByteArrayInputStream(result.getBytes(StandardCharsets.UTF_8))
    (result, stream)
  }

  def parseAndTypeCheck(targetStream: java.io.InputStream): ASTStart =
    generateSyntaxTree(targetStream).flatMap(typeCheckSyntaxTree).get

  def printError(e: Throwable, fileContents: String): Unit = e match {
    case e: ParseException =>
      println("Your program failed parsing.")
      println(e.toString)
      printLocation(e.currentToken.beginLine, e.currentToken.beginColumn, e.currentToken.endColumn, fileContents)
    case e: BlameTokenException =>
      println("Your program failed type checking.")
      println(e.toString)
      printLocation(e.token.beginLine, e.token.beginColumn, e.token.endColumn, fileContents)
    case other => throw other
  }

  def printLocation(beginLine: Int,
                    beginCol: Int,
                    endCol: Int,
                    string: String): Unit = {
    var tabCountBeforeTerm = 0
    (string foldLeft((0, 0))) {
      (pair, c) => (pair, c) match {
        case ((curLine: Int, _: Int), '\n') => (curLine+1, 0)
        case ((curLine: Int, curCol: Int), c) =>
          if (curLine == (beginLine-1)) {
            if (c == '\t' && curCol < (beginCol - 1)) {
              tabCountBeforeTerm += 1
            }
            print(c)
          }
          (curLine, curCol+1)
      }
    }
    println()

    // Bizarrely, JavaCC converts single-character tabs ('\t') to 8 spaces (' ')
    // in its column counts. If we naively trust its column numbers, we'll be wildly
    // off.
    // Therefore, the earlier code tracks the number of tabs that have appeared and we
    // first repeat those back to ensure alignment.
    // To adjust the column headers, we need to subtract 8 for each tab, and then add
    // back the number of tabs (this converts the column numbers back to treating each
    // tab as one column rather than 8).
    val startArrow = (beginCol-1)-7*tabCountBeforeTerm
    val endArrow = endCol-7*tabCountBeforeTerm

    0 until endArrow foreach {
      case i if i < tabCountBeforeTerm => print('\t')
      case i if i >= startArrow => print('^')
      case _ => print(' ')
    }
    print('\n')
  }

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Usage: ./ParallangFrontEnd <filepath>")
      return
    }
    val filepath = args(0);
    val targetStream: InputStream = if (filepath == "system.in") {
      println("Reading from stdin...")
      new BufferedInputStream(System.in);
    } else {
      val initialFile: File = new File(filepath);
      new FileInputStream(initialFile)
    }

    val (fileContents, stream) = readFileFromIOStream(targetStream)

    val syntaxTree: ASTStart = try {
      parseAndTypeCheck(stream)
    } catch {
      case e: Throwable =>
        printError(e, fileContents)
        return
    }
    syntaxTree.dump("")
    println("Type checking succeeded :-)")

    try {
      val pe = new ProcessingElement((0, 0), 1, 1, new CacheHierarchy(0, 100 /* ps */))
      pe.execute(syntaxTree)
      println("======== Execution Terminated ========")
      println(pe.executionContext)
      println("======================================")
    } catch {
      case e => throw e
    }
  }
}
