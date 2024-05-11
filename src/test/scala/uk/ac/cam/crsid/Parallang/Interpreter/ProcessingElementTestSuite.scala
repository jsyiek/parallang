package uk.ac.cam.crsid.Parallang.Interpreter

import org.scalatest.concurrent.TimeLimits.failAfter
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import uk.ac.cam.crsid.Parallang.Interpreter.CommunicationModel.InterconnectionNetwork
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.Exception.OutOfBoundsArrayAccessException
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values._
import uk.ac.cam.crsid.Parallang.Parser.ASTOperator
import uk.ac.cam.crsid.Parallang.TypeChecking.Exception.InvalidAttributeAccessException
import uk.ac.cam.crsid.Parallang.TypeChecking._
import uk.ac.cam.crsid.test_utils.TestUtils.{toExportableStream, toStream}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt

// Note that to test the full ProcessingElement functionality, the InterconnectionNetwork must be used.
// Functionality specific to the InterconnectionNetwork (adding links etc.) is tested there.
class ProcessingElementTestSuite extends AnyFunSuite with TableDrivenPropertyChecks {

  private def getProgramResult[T <: Value](program: java.io.InputStream, t: Type, rowsCols: (Int, Int) = (1, 1)): T = {
    InterconnectionNetwork.resetLengthWidth(rowsCols._1, rowsCols._2)
    InterconnectionNetwork.torus()
    failAfter(1000.millis) {
      InterconnectionNetwork.launchWith(program, throws = true)
      val result = InterconnectionNetwork.recvAsExternal((0, 0), t).v
      assert(result.isInstanceOf[T])
      return result.asInstanceOf[T]
    }
  }

  private def integerValueTest(export: String, expectedValue: Long): Any = integerValueTest("", export, expectedValue)

  private def integerValueTest(preamble: String, export: String, expectedValue: Long, rowsCols: (Int, Int) = (1, 1)): Any = {
    val program = toExportableStream(preamble, export)
    val result = getProgramResult[IntegerValue](program, IntType(), rowsCols)
    assert(result.int === expectedValue)
  }

  private def arrayEquals(a: Array[Value], b: Array[Int]): Boolean = {
    a zip b forall { case (ai, bi) => ai.asInstanceOf[IntegerValue].int == bi }
  }

  /*
   * Arithmetic
   */

  test("Simple addition") {
    integerValueTest("5+5", 10)
  }

  test("Complex arithmetic") {
    integerValueTest(
      "((1000 + 500) * (25 - 10) / (2 + 3)) - (300 + 40) / (5 * 2) + (60 - 30) * 8",
      4706
    )
  }

  test("Negation correctly negates a number") {
    integerValueTest("5 + - 5", 0)
  }


  /*
   * Equality and inequality
   */

  test("Two equal integers compare equal") {
    integerValueTest("5 == 5", 1)
  }

  test("Two unequal integers compare unequal") {
    integerValueTest("1 == 0", 0)
  }

  test("Two equal expressions do not compare unequal") {
    integerValueTest("(3+1*5-2) != (10-4)", 0)
  }

  test("Two unequal expressions equal") {
    integerValueTest("(3+1*5-2)*100 != (10-4)", 1)
  }

  test("Division uses integer division") {
    integerValueTest("5/2", 2)
  }

  test("Two different array refs compare unequal regardless of contents") {
    integerValueTest("{1, 2, 3} == {1, 2, 3}", 0)
  }

  test("An array reference compares equal to itself") {
    integerValueTest(
      "var x: array[int] = {1, 2, 3};",
      "x == x",
      1
    )
  }

  test("(EQ) Two different structure refs compare unequal regardless of contents") {
    integerValueTest(
      "struct dog();",
      "new dog() == new dog()",
      0
    )
  }

  test("(NEQ) Two different structure refs compare unequal regardless of contents") {
    integerValueTest(
      "struct dog();",
      "new dog() != new dog()",
      1
    )
  }


  test("A structure reference compares equal to itself") {
    integerValueTest(
      "struct dog(); var x: struct dog = new dog();",
      "x == x",
      1
    )
  }

  test("Basic TRUE inequality comparisons work") {
    integerValueTest("(1 > 0) + (1 >= 0) + (0 < 1) + (0 <= 1)", 4)
  }

  test("Basic FALSE inequality comparisons work") {
    integerValueTest("(0 > 1) + (0 >= 1) + (1 < 0) + (1 <= 0)", 0)
  }

  /*
   * Boolean expressions
   */
  test("Basic AND expressions work") {
    val table = Table("and expressions",
      ("1 && 1", 1),
      ("0 && 1", 0),
      ("1 && 0", 0),
      ("0 && 0", 0),
      ("(1 + 2) && 1", 1),
      ("(1-1) && (1+1)", 0),
      ("1 >= 0 && 0 <= 1", 1),
      ("1 >= 0 && 0 <= 1 && 0 >= 0 && 0 < 0", 0)
    )
    table foreach {
      case (expression, expectedResult) => integerValueTest(expression, expectedResult)
    }
  }

  test("Logical AND short circuits") {
    integerValueTest(
      "var x: int = 0; fn dog() -> int { var y: int = x; x = x + 1; return y; } dog() && dog()",
      "x",
      1
    )
  }

  test("Basic OR expressions work") {
    val table = Table("or expressions",
      ("1 || 1", 1),
      ("0 || 1", 1),
      ("1 || 0", 1),
      ("0 || 0", 0),
      ("(1 + 2) || 1", 1),
      ("(1-1) || (1+1)", 1),
      ("1 >= 0 || 0 <= 1", 1),
      ("1 >= 0 || 0 <= 1 || 0 >= 0 || 0 < 0", 1)
    )
    table foreach {
      case (expression, expectedResult) => integerValueTest(expression, expectedResult)
    }
  }

  test("Logical OR short circuits") {
    integerValueTest(
      "var x: int = 0; fn dog() -> int { return x = x + 1; } dog() || dog()",
      "x",
      1
    )
  }

  /*
   * Arrays
   */
  test("Can create an array literal") {
    val result = getProgramResult[ArrayValue](
      toExportableStream("{1, 2, 3}"),
      ArrayType(IntType())
    )
    assert(arrayEquals(result.array, Array(1, 2, 3)))
  }

  test("Can export an array auto creation") {
    val result = getProgramResult[ArrayValue](
      toExportableStream("array[int](5, 0)"),
      ArrayType(IntType())
    )
    assert(arrayEquals(result.array, Array(0, 0, 0, 0 ,0)))
  }

  test("Can export an array variable") {
    val result = getProgramResult[ArrayValue](
      toExportableStream("var x: array[int] = array[int](5, 0)", "x"),
      ArrayType(IntType())
    )
    assert(arrayEquals(result.array, Array(0, 0, 0, 0, 0)))
  }

  test("Can index an array literal") {
    val result = getProgramResult[IntegerValue](
      toExportableStream("{0, 1, 2, 3}[2]"),
      IntType()
    )
    assert(result.int === 2)
  }

  test("Can index an array auto creation") {
    val result = getProgramResult[IntegerValue](
      toExportableStream("array[int](5, 0)[2]"),
      IntType()
    )
    assert(result.int === 0)
  }

  test("Can index an array variable") {
    val result = getProgramResult[IntegerValue](
      toExportableStream("var x: array[int] = array[int](5, 0)", "x[2]"),
      IntType()
    )
    assert(result.int === 0)
  }

  test("Can use array as part of bigger arithmetic expression") {
    val result = getProgramResult[IntegerValue](
      toExportableStream("10*{1, 2, 3, 4}[3]/2-2"),
      IntType()
    )
    assert(result.int === 18)
  }

  test("Can call len on array") {
    integerValueTest(
      "len({{1, 2, 3}, {4, 5, 6}, {7, 8, 9}})",
      3
    )
  }

  test("Different arrays compare false (by reference)") {
    integerValueTest(
      "var x: array[int] = {1, 2, 3}; var y: array[int] = {1, 2, 3};",
      "x != y",
      1
    )
  }

  /*
   * Branches
   */
  test("Basic IF-branch checks") {
    val cases = Table("if-cases",
      ("1", "1"),
      ("0", "0"),
      ("1+2+3", "1"),
      ("{1, 2, 3} == {5, 1, 2}", "0")
    )
    cases foreach {
      case (condition, expectedResult) =>
        integerValueTest(
          s"var x: int = 0; if ($condition) { x = 1; }",
          s"x == $expectedResult",
          1
        )
    }
  }

  test("Basic IF-ELSE-branch checks") {
    val cases = Table("if-cases",
      ("1", "1"),
      ("0", "2"),
      ("1+2+3", "1"),
      ("{1, 2, 3} == {5, 1, 2}", "2")
    )
    cases foreach {
      case (condition, expectedResult) =>
        integerValueTest(
          s"var x: int = 1000; if ($condition) { x = 1; } else { x = 2; }",
          s"x == $expectedResult",
          1
        )
    }
  }

  test("Basic IF-ELIF-ELSE-branch checks") {
    val cases = Table("if-cases",
      ("1", "1", "1"),
      ("0", "1", "2"),
      ("1+2+3", "1", "1"),
      ("{1, 2, 3} == {5, 1, 2}", "1", "2"),
      ("0", "0", "3"),
      ("1==2", "2==3", "3")
    )
    cases foreach {
      case (ifCondition, elifCondition, expectedResult) =>
        integerValueTest(
          s"var x: int = 1000; if ($ifCondition) { x = 1; } else if ($elifCondition) { x = 2; } else { x = 3; }",
          s"x == $expectedResult",
          1
        )
    }
  }

  /*
   * Loops
   */

  test("For loop with unmet condition isn't entered") {
    integerValueTest("var x: int = 0; for (0;0;0) { x = 1; }", "x", 0)
  }

  test("For loop for simple first ten integers sum") {
    integerValueTest(
      "var x: int = 0; for (var i: int = 0; i < 10; i = i + 1) { x = x + i + 1; }",
      "x",
      55
    )
  }

  test("For loop can modify external variable in loop conditions") {
    integerValueTest(
      "var x: int = 0; for (x = 1; x < 2000; x = x+x) {}",
      "x",
      2048
    )
  }

  test("While loop with null condition isn't entered") {
    integerValueTest(
      "var x: int = 0; while (0) { x = 1; }",
      "x",
      0
    )
  }

  test("While loop with flag loops until flag is false") {
    integerValueTest(
      "var x: int = 100; while (x) { x = x - 1; }",
      "x",
      0
    )
  }

  /*
   * Functions and calls
   */
  test("Function can return simple integer") {
    integerValueTest(
      "fn dog() -> int { return 5; }",
      "dog()",
      5
    )
  }

  test("Function can take parameter and double it") {
    integerValueTest(
      "fn double(i: int) -> int { return i*2; }",
      "double(5)",
      10
    )
  }

  test("Function can return from a branch") {
    integerValueTest(
      "fn double(i: int) -> int { if (i == 0) { return 100; } return i * 2; }",
      "double(0)",
      100
    )
  }

  test("Recursive functions call themselves recursively") {
    integerValueTest(
      "fn fib(n: int) -> int { if (n <= 1) { return n; } return fib(n-1) + fib(n-2); }",
      "fib(8)",
      21
    )
  }

  test("Function can return from a while loop") {
    integerValueTest(
      "fn dog() -> int { while (1) { return 5; } return 0; }",
      "dog()",
      5
    )
  }

  test("Function can return from a for loop") {
    integerValueTest(
      "fn dog() -> int { for (;;) { return 5; } return 6; }",
      "dog()",
      5
    )
  }

  /*
   * Structures
   */
  test("Can define and initialize a structure") {
    integerValueTest(
      "struct dog(x: int, y: int, z: int)",
      "new dog(0, 1, 2).x + new dog(0, 1, 2).y + new dog(0, 1, 2).z",
      3
    )
  }

  test("Struct value can claim word id") {
    integerValueTest(
      "struct dog(x: int, y: int, z: int); var x: struct dog = new dog(0, 1, 2);",
      "x.x + x.y + x.z",
      3)
  }

  test("Can generate and return a structure from a function") {
    integerValueTest(
      "struct dog(x: int); fn make(x: int) -> struct dog { return new dog(x); }",
      "make(5).x",
      5
    )
  }

  test("Can assign to a structure") {
    integerValueTest(
      "struct dog(x: int); var x: struct dog = new dog(5); x = new dog(6);",
      "x.x",
      6
    )
  }

  test("Can assign to a structure attribute") {
    integerValueTest(
      "struct dog(x: int); var x: struct dog = new dog(5); x.x = 100;",
      "x.x",
      100
    )
  }

  test("Can assign to an array index of a structure attribute") {
    integerValueTest(
      "struct dog(x: array[int]); var x: struct dog = new dog({1, 2, 3}); x.x[2] = 100; x.x[1] = 10; x.x[0] = 1;",
      "x.x[0] + x.x[1] + x.x[2]",
      111
    )
  }

  test("Can assign to multiple layers of attribute/index") {
    integerValueTest(
      "struct fish(x: int); struct cat(f: array[struct fish]); struct dog(c: array[array[struct cat]]); " +
        "var d: struct dog = new dog(array[array[struct cat]](10, {new cat(array[struct fish](10, new fish(10)))}));" +
        "d.c[5][0].f[2].x = 100;",
      "d.c[5][0].f[2].x + d.c[5][0].f[3].x + d.c[2][0].f[2].x",
      120
    )
  }

  test("Can assign to multiple layers of attribute/index WITH INDEX AS FIRST ASSIGNMENT") {
    integerValueTest(
      "struct fish(x: int); var f: array[struct fish] = array[struct fish](10, new fish(10));" +
        "f[0].x = 0; f[1].x = 1; f[2].x = 2;",
      "f[0].x + f[1].x + f[2].x + f[3].x",
      13
    )
  }

  /*
   * Message passing
   */
  test("Can send a message between two cores") {
    integerValueTest(
        "send myX + myY -> worker 0, 0; var x: int = 0; if (myX == 0 && myY == 0) { recv x <- worker 1, 1; }; println(x);",
        "x",
        2,
        (2, 2)
      )
  }

  test("Can broadcast a message") {
    // all nodes except (2, 2) receive 5 and unicast this to (0, 0) which sums and returns the result
    integerValueTest("if (myX == 2 && myY == 2) { send 5 -> broadcast; } else { recv[int] x <- worker 2, 2; send x -> worker 0, 0; }; " +
      "var x: int = 0; " +
      "if (myX == 0 && myY == 0) { " +
        "for (var i: int = 0; i < 3; i = i + 1) {" +
          "for (var j: int = 0; j < 3; j = j + 1) {" +
            "if (i != 2 || j != 2) { recv[int] temp <- worker i, j; x = x + temp;}" +
          "}" +
        "}}",
      "x",
      40,
      (3, 3)
    )
  }

  test("Can broadcast to a row") {
    integerValueTest(
      "if (myY == 2) { send myX -> broadcast_row; } else { recv[int] x <- worker myX, 2; send x -> worker 0, 0; }; " +
        "var x: int = 0; " +
        "if (myX == 0 && myY == 0) { " +
          "for (var i: int = 0; i < 3; i = i + 1) {" +
            // Note here we limit j to 2 as we aren't expecting to recieve anything from the broadcasters.
            "for (var j: int = 0; j < 2; j = j + 1) {" +
              "recv[int] temp <- worker i, j; x = x + temp;" +
            "}" +
          "}}",
      "x",
      0+0+1+1+2+2,
      (3, 3)
    )
  }

  test("Can broadcast to a col") {
    integerValueTest(
      "if (myX == 2) { send myY -> broadcast_col; } else { recv[int] x <- worker 2, myY; send x -> worker 0, 0; }; " +
        "var x: int = 0; " +
        "if (myX == 0 && myY == 0) { " +
          // Note here we limit i to 2 as we aren't expecting to recieve anything from the broadcasters.
          "for (var i: int = 0; i < 2; i = i + 1) {" +
            "for (var j: int = 0; j < 3; j = j + 1) {" +
              "recv[int] temp <- worker i, j; x = x + temp;" +
            "}" +
          "}}",
      "x",
      0 + 0 + 1 + 1 + 2 + 2,
      (3, 3)
    )
  }

  test("Can receive into attribute/index mix") {
    integerValueTest(
      "send 100 -> worker 0, 0; struct fish(x: int); struct cat(fs: array[struct fish]); var x: struct cat = new cat(array[struct fish](5, new fish(5)));" +
        "recv x.fs[1].x <- worker 0, 0;",
      "x.fs[1].x + x.fs[0].x",
      105
    )
  }
  test("Can send a struct as a message between two cores") {
    integerValueTest(
      "struct fish(x: int); send new fish(5) -> worker 0, 0; var y: struct fish = new fish(6); if (myX == 0 && myY == 0) { recv y <- worker 1, 1; }; ",
      "y.x",
      5,
      (2, 2)
    )
  }

  /*
   * Run-time exceptions
   */
  test("Out of bounds array access") {
    val programStream = toStream("{0, 1}[10];")
    InterconnectionNetwork.resetLengthWidth(1, 1)
    assertThrows[OutOfBoundsArrayAccessException](InterconnectionNetwork.launchWith(programStream, throws = true))
  }

  test("Invalid struct attribute access exception") {
    val programStream = toStream("struct fish(x: int); var x: struct fish = new fish(5); x.y;")
    InterconnectionNetwork.resetLengthWidth(1, 1)
    assertThrows[InvalidAttributeAccessException](InterconnectionNetwork.launchWith(programStream, throws = true))
  }

  /*
   * External communication
   */
  test("Can send and receive an integer as an external") {
    val programStream = toStream("recv[int] x <- external; send x -> external;")
    InterconnectionNetwork.resetLengthWidth(1, 1)
    InterconnectionNetwork.sendIntAsExternal((0, 0), 1)
    InterconnectionNetwork.launchWith(programStream)
    assert(InterconnectionNetwork.recvAsExternal((0, 0), IntType()).v.asInstanceOf[IntegerValue].int === 1)
  }

  test("Can send and receive an array as an external") {
    val programStream = toStream("recv[array[array[int]]] x <- external; send x -> external;")
    InterconnectionNetwork.resetLengthWidth(1, 1)
    val a: Array[Any] = Array(Array(1L, 2L, 3L))
    InterconnectionNetwork.sendArrayAsExternal((0, 0), a)
    InterconnectionNetwork.launchWith(programStream)
    val recvAValue = InterconnectionNetwork.recvAsExternal((0, 0), ArrayType(ArrayType(IntType()))).v.asInstanceOf[ArrayValue]
    val recvA = ArrayValue.matrixToScalaArray(recvAValue)
    assert(a.length === recvA.length)
    for (i <- recvA.indices) {
      assert(a(i) === recvA(i))
    }
  }

  test("Can broadcast int as external") {
    val programStream = toStream("recv[int] x <- external; send x -> external;")
    InterconnectionNetwork.resetLengthWidth(1, 1)
    InterconnectionNetwork.broadcastIntAsExternal(1)
    InterconnectionNetwork.launchWith(programStream)
    assert(InterconnectionNetwork.recvAsExternal((0, 0), IntType()).v.asInstanceOf[IntegerValue].int === 1)
  }


  /*
   * Various unit operations
   */
  test("Various unit operations") {
    integerValueTest(
      "var f: unit = unit; f = unit; var g: array[unit] = array[unit](10, unit); var x: int = 10;",
      "x",
      10
    )
  }

  test("Infinity semantics") {
    val testCases: Array[(String, Long)] = Array(
      ("inf+inf;", Long.MaxValue),
      ("inf-inf;", Long.MaxValue),
      ("-inf+inf;", Long.MaxValue),
      ("-inf-5;", -Long.MaxValue),
      ("-inf+5;", -Long.MaxValue),
      ("0*0", 0),
      ("(-inf)*(-inf)", Long.MaxValue),
      ("inf*inf;", Long.MaxValue),
      ("-inf*inf;", -Long.MaxValue),
      ("-inf*5;", -Long.MaxValue),
      ("-inf*-5;", Long.MaxValue),
      ("inf > 1000000;", 1L),
      ("inf < 100000;", 0L),
      ("-inf > -100000;", 0L),
      ("-inf < -100000;", 1L),
    )

    for ((e, exp) <- testCases) {
      integerValueTest(
        s"var x: int = $e",
        "x",
        exp
      )}
  }

  /*
   * Operations on run-time value classes that are illegal (but don't pass type-checking)
   */

  test("Struct initializer value cannot be duplicated") {
    val v = new StructInitializerValue(List(), new mutable.HashMap)
    assertThrows[RuntimeException](v.deepCopy())
  }

  test("Struct initializer can't handle binary operators") {
    val v = new StructInitializerValue(List(), new mutable.HashMap)
    assertThrows[RuntimeException](v.handleBinaryOp(ASTOperator.Operator.EQ, v))
  }

  test("Function value call returns body") {
    var i = 0
    def v(): IntegerValue = {
      i += 1
      new IntegerValue(i)
    }
    val f = new FunctionValue(List(), v)
    assert(f.call().asInstanceOf[IntegerValue].int === 1)
    assert(f.call().asInstanceOf[IntegerValue].int === 2)
  }

  test("Function value cannot be duplicated") {
    def v(): IntegerValue = new IntegerValue(5)
    val f = new FunctionValue(List(), v)

    assertThrows[RuntimeException](f.deepCopy())
  }

  test("Function value cannot handle binary operators") {
    def v(): IntegerValue = new IntegerValue(5)

    val f = new FunctionValue(List(), v)

    assertThrows[RuntimeException](f.handleBinaryOp(ASTOperator.Operator.EQ, f))
  }

  test("Units cannot handle binary op") {
    val u = new UnitValue
    assertThrows[RuntimeException](u.handleBinaryOp(ASTOperator.Operator.PLUS, u))
  }

  test("Units can be deep copied") {
    val u = new UnitValue
    val u2 = u.deepCopy()
  }

  test("Struct value can be converted to string") {
    def s = new StructValue("fish", new mutable.HashMap)
    assert(s.toString === "fish()")
  }

  test("Struct value can abandon word id") {
    def s = new StructValue("fish", new mutable.HashMap)
    s.wordId = 10
    s.abandonWordId()
    assert(s.wordId === -1)
  }

  test("Array value can abandon word id") {
    def a = new ArrayValue(Array(new IntegerValue(5)))
    a.wordId = 10
    a.abandonWordId()
    assert(a.wordId === -1)
  }

  test("Array prints as string correctly") {
    def a = new ArrayValue(Array(new IntegerValue(1), new IntegerValue(2), new IntegerValue(3)))
    assert(a.toString === "{1, 2, 3}")
  }
}
