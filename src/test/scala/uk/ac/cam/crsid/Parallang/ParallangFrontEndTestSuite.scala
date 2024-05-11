package uk.ac.cam.crsid.Parallang

import org.scalatest.funsuite.AnyFunSuite
import uk.ac.cam.crsid.Parallang.Parser.ParseException
import uk.ac.cam.crsid.Parallang.TypeChecking.Exception._
import uk.ac.cam.crsid.Parallang.TypeChecking.{AnyValueMimicType, ArrayType, FunctionType, IntType, NoReturnAnyType, OptionType, StructType, Type, UnitType}
import uk.ac.cam.crsid.test_utils.TestUtils.toStream

import java.io.ByteArrayInputStream
import java.nio.charset.Charset
import scala.collection.mutable

class ParallangFrontEndTestSuite extends AnyFunSuite {

  /*
   * Integers & Arithmetic
   */
  test("Valid arithmetic") {
    val program = toStream("1+2-3;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid complex literal arithmetic") {
    val program = toStream("((1000 + 500) * (25 - 10) / (2 + 3)) - (300 + 40) / (5 * 2) + (60 - 30) * 8;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid addition of array literal and integers") {
    val program = toStream("(1000+500+{1, 2, 3});")
    assertThrows[MismatchedTypeException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid to add two negations") {
    val program = toStream("-5 + -5;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to negate an array") {
    val program = toStream("-{1, 2, 3};")
    assertThrows[MismatchedTypeException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid to negate an array index") {
    val program = toStream("-{1, 2, 3}[0];")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid to negate an attribute") {
    val program = toStream("struct dog(x: int); var d: struct dog = new dog(5); -d.x;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  /*
   * Equality and Inequality
   */
  test("Valid to compare integers with equality/inequality ops") {
    val program = toStream("5 == 5; 5 != 5; 5 < 5; 5 > 5; 5 <= 5; 5 >= 5;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid to compare variables") {
    val program = toStream("var x: int = 5; var y: int = 5; x == y; x != y; x < y; x > y; x <= y; x >= y;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to triple-compare arrays") {
    val program = toStream("{1} == {1} == {1};")
    assertThrows[MismatchedTypeException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid to perform array inequality") {
    val program = toStream("{1} > {1};")
    assertThrows[MismatchedTypeException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  /*
   * Boolean comparisons
   */
  test("Valid to compare integers with boolean operators") {
    val program = toStream("0 || 0; 0 && 0;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to compare arrays") {
    val program = toStream("{1} || {1};")
    assertThrows[MismatchedTypeException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid to conjunction on equality, inequality") {
    val program = toStream("({1} == {1}) && ({1} != {1}) || (0 > 0);")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  /*
   * Arrays
   */

  test("Valid Empty Array") {
    val program = toStream("{};")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Declaring a typed variable with an empty array") {
    val program = toStream("var x: array[int] = {};")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid array with integers") {
    val program = toStream("{1, 2, 3};")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid nested arrays") {
    val program = toStream("{{1, 2, 3}, {4, 5, 6}};")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid array auto-generation") {
    val program = toStream("array[int](5, 5);")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid nested array auto-generation") {
    val program = toStream("array[array[int]](5, array[int](5, 5));")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid incongruent types in array") {
    val program = toStream("{1, {1}};")
    assertThrows[MismatchedTypeException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Incorrectly typed expression for array auto-generation") {
    val program = toStream("array[int](5, array[int](5, 5));")
    assertThrows[MismatchedTypeException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid to index array literal as part of expression") {
    val program = toStream("{1, 2, 3}[1] + 2;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid to index array auto-creation as part of expression") {
    val program = toStream("array[int](5, 5)[0] + 2;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid to index array variable") {
    val program = toStream("var x: array[int] = {1, 2, 3}; x[0];")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid to index array function parameter") {
    val program = toStream("fn dog(y: array[int]) -> int { return y[0]; }")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to index integer literal") {
    val program = toStream("5[0];")
    assertThrows[TypeCantBeIndexedException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid to index integer variable") {
    val program = toStream("var x: int = 5; x[5];")
    assertThrows[TypeCantBeIndexedException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid to index function") {
    val program = toStream("fn dog() -> unit { return unit; } dog[1];")
    assertThrows[TypeCantBeIndexedException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid to assign to lvalue array index") {
    val program = toStream("var x: array[int] = {1, 2, 3}; x[0] = 5; x[0] = x[0] + 5;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to assign invalid type to lvalue array index") {
    val program = toStream("var x: array[int] = {1, 2, 3}; x[0] = {1, 2, 3};")
    assertThrows[MismatchedTypeException](ParallangFrontEnd.parseAndTypeCheck(program));
  }

  /*
   * Structures and Attributes
   */

  test("Invalid to create a structure that doesn't exist") {
    val program = toStream("var dog: struct dog = new dog(5);")
    assertThrows[UndefinedStructException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid to declare a STRUCTURE if another variable with same name in scope") {
    val program = toStream("var dog: int = 5; struct dog();")
    assertThrows[RedeclarationException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid to call 'new' on a non-struct") {
    val program = toStream("fn dog() -> unit {} var cat: int = new dog();")
    assertThrows[NotAStructException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid to initialize struct with wrong number of attributes") {
    val program = toStream("struct dog(); var x: struct dog = new dog(5);")
    assertThrows[IncorrectArgumentNumberException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid to assign to an attribute") {
    val program = toStream("struct dog(x: int); var x: struct dog = new dog(0); x.x = 5;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid to assign to index of an attribute") {
    val program = toStream("struct dog(x: array[int]); var x: struct dog = new dog({0}); x.x[0] = 5;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to assign non-array for array attribute") {
    val program = toStream("struct dog(x: array[int]); var x: struct dog = new dog({0}); x.x = 5;")
    assertThrows[MismatchedTypeException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid complex assignment to attribute chain") {
    val program = toStream(
      "struct cat(x: option[int]); struct dog(c: array[struct cat]); " +
      "struct zebra(d: array[struct dog]); " +
      "var x: struct zebra = new zebra(array[struct dog](5, new dog(array[struct cat](6, new cat(some(5))))));" +
      "x.d[3].c[1].x = none;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid complex assignment to attribute chain") {
    val program = toStream(
      "struct cat(x: option[int]); struct dog(c: array[struct cat]); " +
      "struct zebra(d: array[struct dog]); " +
      "var x: struct zebra = new zebra(array[struct dog](5, new dog(array[struct cat](6, new cat(some(5))))));" +
      "x.d[3].c[1].x = 0;")
    assertThrows[MismatchedTypeException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid to access non-existent attribute") {
    val program = toStream("struct dog(x: int, y: int, z:int); var x: struct dog = new dog(1, 2, 3); x.w;")
    assertThrows[InvalidAttributeAccessException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid to access non-existent attribute AFTER AN INDEX") {
    val program = toStream(
      "struct cat();" +
      "struct dog(x: array[struct cat]);" +
      "var y: struct dog = new dog({new cat()});" +
      "y.d[0].c;")
    assertThrows[InvalidAttributeAccessException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid to access as part of expression") {
    val program = toStream("struct dog(x: int); new dog(5).x + new dog(6).x;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to access attribute of non-struct") {
    val program = toStream("var x: int = 5; x.y;")
    assertThrows[NotAStructException](ParallangFrontEnd.parseAndTypeCheck(program));
  }

  /*
   * Variables and declarations
   */
  test("Invalid undefined variable access") {
    val program = toStream("x;")
    assertThrows[UndefinedVariableException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid variable access after declaration") {
    val program = toStream("var x: int = 5; x;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to redeclare second variable with same name") {
    val program = toStream("var x: int = 5; var x: int = 5;")
    assertThrows[RedeclarationException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  /*
   * Functions, return, and calls
   */
  test("Valid to call unit function with no return statement") {
    val program = toStream("fn dog() -> unit {} var cat: unit = dog();")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to declare a function if another variable with same name in scope") {
    val program = toStream("var dog: int = 5; fn dog() -> unit {}")
    assertThrows[RedeclarationException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid function with missing return") {
    val program = toStream("fn dog () -> int {}")
    assertThrows[MissingReturnPathException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid function with return only in if and elif") {
    val program = toStream("fn dog() -> int { if (1) { return 5; } else if (0) { return 6; }; }")
    assertThrows[MissingReturnPathException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid function with return in an if, elif, and else") {
    val program = toStream("fn dog() -> int { if (1) { return 5; } else if (2) { return 6; } else { return 7; };}")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid function with return in a for loop") {
    val program = toStream("fn dog() -> int { for (0;0;0) { return 5; }; }")
    assertThrows[MissingReturnPathException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid function with return after for loop") {
    val program = toStream("fn dog() -> int { for (0;0;0) {}; return 5; }")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid function with if statement inside of for loop with return") {
    val program = toStream("fn dog() -> int { for (0;0;0) { if (0) { return 0; } } }")
    assertThrows[MissingReturnPathException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid function with if statement inside of while loop with return") {
    val program = toStream("fn dog() -> int { while (0) { if (0) { return 0; } } }")
    assertThrows[MissingReturnPathException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid to have top-level return") {
    val program = toStream("return unit;")
    assertThrows[ToplevelReturnException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid function with multiple parameters") {
    val program = toStream("fn dog(x: int, y: array[int]) -> unit {}")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid function with multiple parameters can refer to them in code") {
    val program = toStream("fn dog(x: int, y: array[int]) -> unit { y[0] = x; }")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to call a struct type") {
    val program = toStream("struct dog(); dog();")
    assertThrows[NotCallableException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid to call an integer") {
    val program = toStream("var x: int = 5; x();")
    assertThrows[NotCallableException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid to call an array") {
    val program = toStream("var x: array[int] = {1, 2, 3}; x();")
    assertThrows[NotCallableException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid to call an undefined function") {
    val program = toStream("poodle(1, 2, 3);")
    assertThrows[UndefinedVariableException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid to call function with the wrong number of arguments") {
    val program = toStream("fn dog(a: int) -> int { return 5; } dog(1, 2);")
    assertThrows[IncorrectArgumentNumberException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  /*
   * Loops
   */
  test("Valid simple while loop") {
    val program = toStream("while (1) { }")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid simple for loop") {
    val program = toStream("for (;;) { }")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid nested for loop") {
    val program = toStream("for (;;) { for (;;) { } }")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to use declaration in for loop scope outside of scope") {
    val program = toStream("for (var x: int = 0; x < 1; x = x + 1) { } x;")
    assertThrows[UndefinedVariableException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid to use declaration in while loop scope outside of scope") {
    val program = toStream("while (0) { var x: int = 0; } x;")
    assertThrows[UndefinedVariableException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  /*
   * Message passing
   */
  test("Valid to send an integer") {
    val program = toStream("send 5 -> worker 5, 5;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid to broadcast an integer") {
    val program = toStream("send 5 -> broadcast;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid to send a defined variable") {
    val program = toStream("var x: int = 5; send x -> worker 5, 5;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to use arrays for worker indices") {
    val program = toStream("send 5 -> worker {1}, {2};")
    assertThrows[MismatchedTypeException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid to send complex expression") {
    val program = toStream("var x: int = 5; var y: int = 6; send x+(x*y-y/x) -> worker x+y, x*y;")
    ParallangFrontEnd.parseAndTypeCheck(program);
  }

  test("Invalid to send a struct *type*") {
    val program = toStream("struct dog(); send dog -> worker 5, 5;")
    assertThrows[MismatchedTypeException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid to send a struct *value*") {
    val program = toStream("struct dog(); var x: struct dog = new dog(); send x -> worker 5, 5;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to send a function") {
    val program = toStream("fn dog() -> unit {}; send dog -> worker 5, 6;")
    assertThrows[MismatchedTypeException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid to receive into an rvalue") {
    val program = toStream("recv 5 <- worker 5, 6;")
    assertThrows[ParseException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid to receve into an lvalue") {
    val program = toStream("var x: int = 0; recv x <- worker 5, 6;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid to recv-declare with a new name") {
    val program = toStream("recv[array[array[int]]] x <- worker 0, 0;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to receive-declare with an EXISTING name") {
    val program = toStream("var x: int = 5; recv[int] x <- worker 0, 0;")
    assertThrows[RedeclarationException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  /*
   * Built-ins
   */

  test("Valid to call len on array") {
    val program = toStream("len({1, 2, 3});")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to call len on a non-array") {
    val program = toStream("len(1);")
    assertThrows[MismatchedTypeException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid to call get on an option") {
    val program = toStream("get(some(1));")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to call get on a non-option") {
    val program = toStream("get(1);")
    assertThrows[NotAnOptionException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid to call println on an integer") {
    val program = toStream("println(1);")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid to call println on a defined variable") {
    val program = toStream("var x: int = 5; println(x);")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Invalid to call println on a function") {
    val program = toStream("fn dog() -> unit {} println(dog);")
    assertThrows[MismatchedTypeException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Invalid to call println on a struct *type*") {
    val program = toStream("struct dog(); println(dog);")
    assertThrows[MismatchedTypeException](ParallangFrontEnd.parseAndTypeCheck(program))
  }

  test("Valid to use myX and myY in an integer context") {
    val program = toStream("myX + myY + 5;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Valid to use numRows and numCols in an integer context") {
    val program = toStream("NumRows + NumCols;")
    ParallangFrontEnd.parseAndTypeCheck(program)
  }

  test("Function type does not match other values") {
    val f = FunctionType(IndexedSeq(IntType()), IntType())
    val types = Array(
      IntType(),
      ArrayType(IntType()),
      StructType("foo", new mutable.LinkedHashMap[String, Type]()),
      NoReturnAnyType(),
      AnyValueMimicType(),
      f
    )
    for (t <- types) {
      assert(!f.matches(t))
    }
  }

  test("AnyValueMimicType changes its toString when matched") {
    val a = AnyValueMimicType()
    assert(a.toString === "<...>")
    a.matches(IntType())
    assert(a.toString === "AnyValueMimicType: int")
  }

  test("NoReturnAnyType matches all types") {
    val types = Array(
      IntType(),
      ArrayType(IntType()),
      StructType("foo", new mutable.LinkedHashMap[String, Type]()),
      NoReturnAnyType(),
      AnyValueMimicType(),
      FunctionType(IndexedSeq(IntType()), IntType())
    )
    val r = NoReturnAnyType()
    for (t <- types) {
      assert(r.matches(t))
    }
  }

  test("Correct print statements") {
    val params = new mutable.LinkedHashMap[String, Type]()
    params.put("bar", IntType())
    val t = Array(
      (NoReturnAnyType(), "<Empty Type>"),
      (StructType("foo", params), "struct foo (bar: int)"),
      (StructType("foo", new mutable.LinkedHashMap[String, Type](), isInitializer = true), "StructType foo ()"),
      (OptionType(IntType()), "option[int]")
    )

    for ((t1, name) <- t) {
      assert(t1.toString === name)
    }
  }

  test("UnitType defers match check to other class") {
    val u = UnitType()
    val i = IntType()
    assert(!u.matches(i))
  }
}
