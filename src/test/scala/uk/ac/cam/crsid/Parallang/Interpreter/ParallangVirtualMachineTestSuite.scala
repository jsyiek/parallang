package uk.ac.cam.crsid.Parallang.Interpreter

import org.scalatest.concurrent.TimeLimits.failAfter
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import uk.ac.cam.crsid.Parallang.Interpreter.CommunicationModel.LatencyParameterSet.overhead_ps
import uk.ac.cam.crsid.Parallang.Interpreter.CommunicationModel.{Datacenter, HighPowerInternet, InterconnectionNetwork, LatencyParameterSet, MulticoreComputer}
import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.{ALUEvent, LatencyEvent, MessageReadEvent}
import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.Listeners.TimeTallyListener
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Listeners.{CurrentMemoryListener, PeakMemoryListener}
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.Value.MIN_HEAP_WORD
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.{ArrayValue, IntegerValue, Value}
import uk.ac.cam.crsid.lib.Events.Listeners.TallyListener
import uk.ac.cam.crsid.test_utils.TestUtils.toStream

import scala.concurrent.duration.DurationInt


// BE CAREFUL!
// YOU CANNOT TRUST `AllocationMemoryEvent` AND `DeallocationMemoryEvent` TO SHOW MEMORY LEAKS
// THEY DO NOT NECESSARILY EQUAL THANKS TO EMPTY ARRAYS
// CONSIDER BELOW PROGRAM:
// var a: array[array[int]] = array[array[int]](1, {});
// a[0] = array[int](1, 0);
// YOU, CORRECTLY, SEE:
// first line:
// {{}} -> allocate (+ pay heap)
// {}   -> allocate
//
// second line:
// {0} -> allocate (+ pay heap)
// 0   -> allocate
//
// end of program:
// {{0}} -> deallocate (- pay heap)
// {0}   -> deallocate (- pay heap)
// 0     -> deallocate
//
// total: 4 allocate
//        3 deallocate
//
// ONLY LATENCYEVENT TALLIES (`AllocationEvent`, `DeallocationEvent`) CAN PROVE MEMORY LEAKS

class ParallangVirtualMachineTestSuite extends AnyFunSuite with TableDrivenPropertyChecks {

  private def peakMemoryTest(program: String, expectedPeakMemory: Long): Unit = {
    val programStream = toStream(program)

    val peakMemoryListener = new PeakMemoryListener

    var i = 0
    InterconnectionNetwork.resetLengthWidth(1, 1)
    InterconnectionNetwork.torus()
    InterconnectionNetwork.addGlobalMemoryListener(peakMemoryListener)
    failAfter(1000.millis) {
      InterconnectionNetwork.launchWith(programStream)
    }
    InterconnectionNetwork.printSummaryDiagnostics()

    assert(peakMemoryListener.peak === expectedPeakMemory)
  }

  private def memoryLeakAvoidanceTest(program: String, dimension: (Int, Int) = (1, 1)): Unit = {
    val programStream = toStream(program)

    val latencyTallyListenerArray = Array.tabulate[TallyListener[LatencyEvent]](dimension._1 * dimension._2) { _ => new TallyListener }
    val currentMemoryListenerArray = Array.tabulate[CurrentMemoryListener](dimension._1 * dimension._2) { _ => new CurrentMemoryListener }


    InterconnectionNetwork.resetLengthWidth(dimension._1, dimension._2)
    var i = 0
    InterconnectionNetwork.addGlobalLatencyListener({
      i += 1
      latencyTallyListenerArray(i - 1)
    })
    i = 0
    InterconnectionNetwork.addGlobalMemoryListener({
      i += 1;
      currentMemoryListenerArray(i - 1)
    })
    InterconnectionNetwork.torus()
    failAfter(1000.millis) {
      InterconnectionNetwork.launchWith(programStream)
    }
    InterconnectionNetwork.printSummaryDiagnostics()

    assert(currentMemoryListenerArray forall (_.currentMemory === 0))
    assert(latencyTallyListenerArray forall (t => t.tallies("AllocationEvent") === t.tallies("DeallocationEvent")))
  }

  /*
   * Returns an array of the listeners
   */
  private def latencyTest(program: String, listener: => TallyListener[LatencyEvent], rowsCols: (Int, Int) = (1, 1)): Array[TallyListener[LatencyEvent]] = {
    val programStream = toStream(program)

    val listenerArray = Array.tabulate[TallyListener[LatencyEvent]](rowsCols._1 * rowsCols._2) { _ => listener }

    var i = 0
    InterconnectionNetwork.resetLengthWidth(rowsCols._1, rowsCols._2)
    InterconnectionNetwork.torus()
    InterconnectionNetwork.addGlobalLatencyListener({ i += 1; listenerArray(i-1) })
    failAfter(1000.millis) {
      InterconnectionNetwork.launchWith(programStream)
    }
    InterconnectionNetwork.printSummaryDiagnostics()
    listenerArray
  }

  private def tallyCountTest(program: String, event: String, expectedCount: Int): Unit = {
    val listeners = latencyTest(program, new TallyListener[LatencyEvent])
    assert(listeners(0)(event) === expectedCount)
  }

  private def parallelTallyCountTest(program: String, dimension: (Int, Int) = (1, 2), event: String, expectedCount: Int): Unit = {
    val listeners = latencyTest(program, new TallyListener[LatencyEvent], rowsCols=dimension)
    assert(listeners(0)(event) === expectedCount)
  }

  val simpleLatencyTestCases = Table("Simple Test Cases - name, program, event, and expected count",

    /* ALU Tests */
    ("1x Add = 1x ALUEvent", "1+1;", "ALUEvent", 1),
    ("5x Add = 5x ALUEvent", "1+1+1+1+3+4;", "ALUEvent", 5),
    ("6x Arbitrary Arithmetic = 6x ALUEvent", "(1+1)*2/5-2*(3-2);", "ALUEvent", 6),
    ("6x Comparison = 6x ALUEvent", "1 == 1; 1 != 1; 1 <= 1; 1 >= 1; 1 < 1; 1 > 1;", "ALUEvent", 6),
    ("Complex heavy use of all operators", "(1 > 1) - 2*3/5 > 10 - 10 == 20 >= 2 <= 2;", "ALUEvent", 9),

    /* Allocation tests */
    ("Anonymous Int = 0x AllocationEvent", "1+1;", "AllocationEvent", 0),
    ("INTEGER added to constant = 0x AllocationEvent", "var x: int = 1; x = x + 1;", "AllocationEvent", 0),
    ("Array constant = 0x AllocationEvent", "{1, 1, 1}[0];", "AllocationEvent", 0),
    ("Auto-created array constant = 0x AllocationEvent", "array[int](10, 10)[0];", "AllocationEvent", 0),
    ("Array tied to variable = 1x AllocationEvent (and not once for each element)", "var x: array[int] = {1, 2, 3};", "AllocationEvent", 1),
    ("2x arr nested in arr = 3x block allocation event", "var x: array[array[int]] = {{1}, {2}};", "AllocationEvent", 3),
    ("Auto-created array tied to variable = 1x AllocationEvent (and not once for each element)", "var x: array[int] = array[int](10, 10);", "AllocationEvent", 1),
    ("Copy ARRAY out of array = 11x AllocationEvent (11x to block allocate initial array, 0x to ref copy array out)", "var x: array[array[int]] = array[array[int]](10, {1, 2, 3}); var y: array[int] = x[0];", "AllocationEvent", 11),
    ("Allocating a structure = 1x AllocationEvent", "struct dog(x: int, y: int); var x: struct dog = new dog(5, 5);", "AllocationEvent", 1),
    ("Variable to array, then copy integer from it = 1x AllocationEvent", "var x: array[int] = {1, 2, 3}; var y: int = x[0];", "AllocationEvent", 1),
    ("Copy ARRAY out of array = 1x AllocationEvent (3 for 2x3 array, none for reference copy)", "var x: array[array[int]] = array[array[int]](2, {1, 2, 3}); var y: array[int] = x[0];", "AllocationEvent", 3),
    ("Function arguments initialized by copy", "fn x(y: int, z: int) -> unit {}; var a: int = 1; var b: int = 2; x(a, b);", "AllocationEvent", 0),
    ("Array fn arguments initialize by ref copy", "fn x(y: array[int]) -> unit {}; var a: array[int] = {1, 2, 3}; x(a);", "AllocationEvent", 1),
    ("Array fn arguments initialize by ref copy: test with nested arrays", "fn x(y: array[array[int]]) -> unit {}; var a: array[array[int]] = {{1}, {2}}; x(a);", "AllocationEvent", 3),
    ("ALLOCATION Regression test for heap aliasing/dealiasing", "var a: array[array[int]] = array[array[int]](10, array[int](10, 10));\na[5] = array[int](20, 10);\na[4][0] = 4;\na[5][0] = 5;\na[5] = a[4];", "AllocationEvent", 12),

    /* Deallocation tests */
    ("Anonymous values = 0x DeallocationEvent", "{1, 2, 3}; 1+1; array[int](10, 10);", "DeallocationEvent", 0),
    ("Array of 5 integers = 1x DeallocationEvent", "var x: array[int] = {1, 2, 3, 4, 5};", "DeallocationEvent", 1),
    ("Array of 5 arrays = 6x DeallocationEvent", "var x: array[array[int]] = {{1}, {2}, {3}, {4}, {5}};", "DeallocationEvent", 6),
    ("Array of struct = lots of DeallocationEvent", "struct dog(x: int, y: int); var x: array[struct dog] = {new dog(1, 2), new dog(3, 4)}; ", "DeallocationEvent", 3), // 1x array 2x structure. integers aren't reported as they aren't by reference.
    ("Function stack-locals deallocated on exit", "fn x() -> unit { var x: int = 0; var y: int = 0; }; x();", "DeallocationEvent", 0),
    ("Function heap-locals deallocated on exit", "fn x() -> unit { var x: array[int] = {1}; var y: array[int] = {1, 2}; }; x();", "DeallocationEvent", 2),
    ("If-statement stack-deallocation", "if (1) { var x: int = 0; }", "DeallocationEvent", 0),
    ("For-statement stack-deallocation", "for (var y: int = 0; y < 10; y = y + 1) { var x: int = 0; }", "DeallocationEvent", 0),
    ("If-statement heap-deallocation", "if (1) { var x: array[int] = {1}; }", "DeallocationEvent", 1),
    ("For-statement heap-deallocation", "for (var y: int = 0; y < 10; y = y + 1) { var x: array[int] = {1}; }", "DeallocationEvent", 10),
    ("Structure deallocations", "struct cat(z: int); struct dog(x: int, y: struct cat); var a: struct dog = new dog(1, new cat(2));", "DeallocationEvent", 2),
    ("DEALLOCATION Regression test for heap aliasing/dealiasing", "var a: array[array[int]] = array[array[int]](10, array[int](10, 10));\na[5] = array[int](20, 10);\na[4][0] = 4;\na[5][0] = 5;\na[5] = a[4];", "DeallocationEvent", 12),

    /* Branching events */
    ("If statement = 1x BranchEvent", "if (1) {};", "BranchEvent", 1),
    ("If-Elif = 2x BranchEvent", "if (0) {} else if (1) {};", "BranchEvent", 2),
    ("If-Elif-Else = 2x BranchEvent", "if (0) {} else if (0) {} else {};", "BranchEvent", 2),
    ("For = BranchEvent equal to loop count plus one", "for (var i: int = 0; i < 10; i = i + 1) {}", "BranchEvent", 11),
    ("While = BranchEvent equal to loop count plus one", "var i: int = 10; while (i != 0) { i = i - 1; }", "BranchEvent", 11),
    ("Unentered while loop = 1x BranchEvent", "var i: int = 0; while (i != 0) { i = i - 1; }", "BranchEvent", 1),
    ("Unentered for loop = 1x BranchEvent", "for (var i: int = 0; i != 0; i = i - 1) { i = i - 1; }", "BranchEvent", 1),
    ("Entering function endures 2x branch", "fn f() -> unit {}; f();", "BranchEvent", 2),
    ("Recursive function (with an if-statement)", "fn f(i: int) -> unit {if (i == 0) { return unit; } return f(i-1);}; f(10);", "BranchEvent", 33),

    /* Memory lookup events */
    // these are tested as part of the cache model integration tests
  )

  for ((name, program, eventType, expectedCount) <- simpleLatencyTestCases) {
    test(name) {
      tallyCountTest(program, eventType, expectedCount)
    }
    test("Memory Leak Avoidance: " + name) {
      memoryLeakAvoidanceTest(program)
    }
  }

  /* Message send/read tests */
  val parallelSimpleLatencyTests = Table("Parallel test cases - name, program, dimension, event, expectedCount",
    ("Single Message Send", "if ( myX == 0 && myY == 0 ) { send 1 -> worker 0, 1; }", (1, 2), "MessageSendEvent", 1),
    ("Single Message Receive", "if ( myX == 0 && myY == 1 ) { send 1 -> worker 0, 0; } else { recv[int] x <- worker 0, 1; }", (1, 2), "MessageReadEvent", 1),
    ("Ten sent messages", "if ( myX == 0 && myY == 0 ) { for (var i: int = 0; i < 10; i = i + 1) { send 1 -> worker 0, 1; } }", (1, 2), "MessageSendEvent", 10),
    ("Ten received messages", "for (var i: int = 0; i < 10; i = i + 1) { if ( myX == 0 && myY == 1 ) { send 1 -> worker 0, 0; } else { recv[int] x <- worker 0, 1; } }", (1, 2), "MessageReadEvent", 10),
    ("Message sent over fowarding path", "if ( myX == 0 && myY == 0 ) { send 1 -> worker 3, 3; }", (4, 4), "MessageSendEvent", 1),
    ("Message received over fowarding path", "if ( myX == 3 && myY == 3 ) { send 1 -> worker 0, 0; } else if (myX == 0 && myY == 0) { recv[int] x <- worker 3, 3; }", (4, 4), "MessageReadEvent", 1),
    ("Array allocated when received",
      "var a: array[array[int]] = {}; " +
      "if ( myX == 3 && myY == 3 ) { " +
        "a = array[array[int]](10, array[int](10, 10)); " +
        "send a -> worker 0, 0; " +
      "} else if (myX == 0 && myY == 0) { " +
        "recv a <- worker 3, 3; " +
      "}", (4, 4), "AllocationEvent", 11),
  )

  for ((name, program, dimension, eventType, expectedCount) <- parallelSimpleLatencyTests) {
    test(name) {
      parallelTallyCountTest(program, dimension, eventType, expectedCount)
    }
    test("Memory Leak Avoidance: " + name) {
      memoryLeakAvoidanceTest(program, dimension)
    }
  }

  /* Bandwidth-latency model tests */
  test("Latency calculation correct over single hop") {
    val programStream = toStream("if (myY == 1) { send 1 -> worker 0, 0; } else { recv[int] x <- worker 0, 1; }")

    val listenerArray = Array.tabulate[TimeTallyListener](1 * 2) { _ => new TimeTallyListener }

    var i = 0
    InterconnectionNetwork.resetLengthWidth(1, 2)
    InterconnectionNetwork.torus()
    InterconnectionNetwork.addGlobalLatencyListener({
      i += 1;
      listenerArray(i-1)
    })

    val currentMemoryListenerArray = Array.tabulate[CurrentMemoryListener](1 * 2) { _ => new CurrentMemoryListener }
    i = 0
    InterconnectionNetwork.addGlobalMemoryListener({
      i += 1; currentMemoryListenerArray(i - 1)
    })

    failAfter(1000.millis) {
      InterconnectionNetwork.launchWith(programStream)
    }
    InterconnectionNetwork.printSummaryDiagnostics()

    // Here, the work performed by the second machine is less than the time it takes to deliver.
    // The receiving machine must catch up.
    // In this case, no further work is performed by the sending machine so it is just the cost of bandwidth (minimum of 1 ns) and the hop latency.
    assert(listenerArray(0).tallies("MessageReadEvent")
      === InterconnectionNetwork.getLatencyParameterSet.getWirePushTime(new IntegerValue(1))
          + InterconnectionNetwork.getLatencyParameterSet.getWireLatencyTime(hopCount = 1)
          + 2*overhead_ps
    )

    // 4 bytes is below the resolution at which FiniteDuration can calculate realistic wire push times.
    // For this reason, we default to 1.nanoseconds to avoid the floating point underflow of 0.nanoseconds.
    assert(listenerArray(1).tallies("MessageSendEvent") ===
      InterconnectionNetwork.getLatencyParameterSet.getWirePushTime(new IntegerValue(1)) + overhead_ps
    )
    assert(currentMemoryListenerArray forall (_.currentMemory === 0))
  }

  test("All parameter sets work as expecteed") {
    val parameterSets = Array(Datacenter, HighPowerInternet, MulticoreComputer)
    for (p <- parameterSets) {
      val programStream = toStream("if (myY == 1) { send 1 -> worker 0, 0; } else { recv[int] x <- worker 0, 1; }")

      val listenerArray = Array.tabulate[TimeTallyListener](1 * 2) { _ => new TimeTallyListener }

      var i = 0
      InterconnectionNetwork.resetLengthWidth(1, 2)
      InterconnectionNetwork.setLatencyParameterSet(p)
      InterconnectionNetwork.torus()
      InterconnectionNetwork.addGlobalLatencyListener({
        i += 1;
        listenerArray(i - 1)
      })

      val currentMemoryListenerArray = Array.tabulate[CurrentMemoryListener](1 * 2) { _ => new CurrentMemoryListener }
      i = 0
      InterconnectionNetwork.addGlobalMemoryListener({
        i += 1;
        currentMemoryListenerArray(i - 1)
      })

      failAfter(1000.millis) {
        InterconnectionNetwork.launchWith(programStream)
      }
      InterconnectionNetwork.printSummaryDiagnostics()

      // Here, the work performed by the second machine is less than the time it takes to deliver.
      // The receiving machine must catch up.
      // In this case, no further work is performed by the sending machine so it is just the cost of bandwidth (minimum of 1 ns) and the hop latency.
      assert(listenerArray(0).tallies("MessageReadEvent")
        === InterconnectionNetwork.getLatencyParameterSet.getWirePushTime(new IntegerValue(1))
        + InterconnectionNetwork.getLatencyParameterSet.getWireLatencyTime(hopCount = 1)
        + 2 * overhead_ps
      )

      // 4 bytes is below the resolution at which FiniteDuration can calculate realistic wire push times.
      // For this reason, we default to 1.nanoseconds to avoid the floating point underflow of 0.nanoseconds.
      assert(listenerArray(1).tallies("MessageSendEvent") ===
        InterconnectionNetwork.getLatencyParameterSet.getWirePushTime(new IntegerValue(1)) + overhead_ps
      )
      assert(currentMemoryListenerArray forall (_.currentMemory === 0))

    }
  }

  test("Latency calculation correct when the sender performs additional work before sending") {
    val programStream = toStream("if (myY == 1) { 1+1; 1+1; 1+1; send 1 -> worker 0, 0; } else { recv[int] x <- worker 0, 1; }")
    val listenerArray = Array.tabulate[TimeTallyListener](1 * 2) { _ => new TimeTallyListener }

    var i = 0
    InterconnectionNetwork.resetLengthWidth(1, 2)
    InterconnectionNetwork.torus()
    InterconnectionNetwork.addGlobalLatencyListener({ i += 1; listenerArray(i-1) })

    val currentMemoryListenerArray = Array.tabulate[CurrentMemoryListener](1 * 2) { _ => new CurrentMemoryListener }
    i = 0
    InterconnectionNetwork.addGlobalMemoryListener({
      i += 1;
      currentMemoryListenerArray(i - 1)
    })

    failAfter(1000.millis) {
      InterconnectionNetwork.launchWith(programStream)
    }
    InterconnectionNetwork.printSummaryDiagnostics()

    assert(listenerArray(0).tallies("MessageReadEvent")
      === 3 * ALUEvent().penalty_ps + InterconnectionNetwork.getLatencyParameterSet.getWirePushTime(new IntegerValue(1))
          + InterconnectionNetwork.getLatencyParameterSet.getWireLatencyTime(1)
          + 2*overhead_ps
    )

    assert(listenerArray(1).tallies("MessageSendEvent")
      === InterconnectionNetwork.getLatencyParameterSet.getWirePushTime(new IntegerValue(1)) + overhead_ps
    )

    assert(currentMemoryListenerArray forall (_.currentMemory === 0))
  }

  test("2-length forwarding path, simple workload") {
    // include branch and 2x 1+1 to ensure event times are equal between the two branches at the time of the send/recv
    // instruction.
    val programStream = toStream("if (myY == 1 && myX == 1) { if (1) {}; 1+1; 1+1; send 1 -> worker 0, 0; } else if (myX == 0 && myY == 0) { recv[int] x <- worker 1, 1; }")
    val listenerArray = Array.tabulate[TimeTallyListener](3 * 3) { _ => new TimeTallyListener }

    var i = 0
    InterconnectionNetwork.resetLengthWidth(3, 3)
    InterconnectionNetwork.torus()
    InterconnectionNetwork.addGlobalLatencyListener({ i += 1; listenerArray(i-1) })

    val currentMemoryListenerArray = Array.tabulate[CurrentMemoryListener](3 * 3) { _ => new CurrentMemoryListener }
    i = 0
    InterconnectionNetwork.addGlobalMemoryListener({
      i += 1;
      currentMemoryListenerArray(i - 1)
    })

    failAfter(1000.millis) {
      InterconnectionNetwork.launchWith(programStream)
    }
    InterconnectionNetwork.printSummaryDiagnostics()



    /* Fastest path shown with == and ||
     *     |         |         |
     * - (0, 0) == (0, 1) -- (0, 2) -
     *     |         ||        |
     * - (1, 0) -- (1, 1) -- (1, 2) -
     *     |         |         |
     * - (2, 0) -- (2, 1) -- (2, 2) -
     *     |         |         |
     */

    assert(listenerArray(0).tallies("MessageReadEvent")
      === InterconnectionNetwork.getLatencyParameterSet.getWirePushTime(new IntegerValue(1))
      + InterconnectionNetwork.getLatencyParameterSet.getWireLatencyTime(2)
      + 2*overhead_ps
    )
    assert(currentMemoryListenerArray forall (_.currentMemory === 0))
  }

  val parallelSimpleMemoryTests = Table("Simple Test Cases - name, program, expected peak memory (words)",
    ("Empty program", "0;", 0),
    ("Integer", "var x: int = 1;", 1),
    ("3-Array literal", "var x: array[int] = {0, 0, 0};", 4+1), // +1 is array length
    ("3-array auto-creation", "var x: array[int] = array[int](3, 0);", 4+1), // +1 is array length
    ("Simple struct", "struct X(y: int, z: int); var x: struct X = new X(10, 10);", 3),
    ("Assignment: array", "var a: array[int] = array[int](10, 10); var b: array[int] = array[int](10, 10); " +
      "var c: array[int] = array[int](10, 10); " +
      "fn f(a: array[int], b: array[int], c: array[int]) -> unit { a = c; b = c; }; " +
      "f(a, b, c); f(a, b, c);", 3*(2+10)+2*3),
    ("Assignment and function: array", "var a: array[int] = array[int](10, 10); var b: array[int] = array[int](10, 10); " +
      "var c: array[int] = array[int](10, 10); " +
      "fn f(a: array[int], b: array[int], c: array[int]) -> unit { a = c; b = c; }; " +
      "f(a, b, c); f(a, b, c); ",
      3*(2+10)+3*2
    ),
    ("Nesting: struct", "struct Y(w: int); struct X(y: struct Y, z: int); var x: struct X = new X(new Y(0), 0);", 4),
    ("Nesting & sequence: array", "if (1) { var x: array[array[int]] = array[array[int]](10, array[int](10, 10)); } if (1) { var x: array[array[int]] = array[array[int]](10, array[int](10, 10)); }", 2*11+100),
    ("Sequence: Two defined integers", "var x: int = 10; var y: int = 20;", 2),
    ("Sequence: Two defined arrays", "var x: array[int] = {0, 0, 0}; var y: array[int] = {1, 1, 1};", 10),
    ("Sequence: Two defined structs", "struct X(y: int, z: int); var x1: struct X = new X(0, 0); var x2: struct X = new X(0, 0);", 6),
    ("Scoping (If): 2 integers, but only 1 active at a time", "if (1) { var x: int = 10; }; if (1) { var y: int = 10; };", 1),
    ("Scoping (For): 3 integers, but only 2 active at a time", "for (var i: int = 0; i < 2; i = i + 1) { var x: int = 0; }", 2),
    ("Scoping (While): 3 integers, but only 2 active at a time", "var i: int = 0; while (i < 2) { i = i + 1; var x: int = 0; }", 2),
    ("Scoping (Function): 2 integers, but only 1 active at a time", "fn foo() -> unit { var x: int = 0; }; foo(); foo();", 1),
    ("Function duplicates arguments", "fn foo(x: int) -> unit {}; var y: int = 0; foo(y);", 2),
    ("Function copies reference", "fn foo(x: array[int]) -> unit {}; var y: array[int] = {0, 1}; foo(y);", 6),
    ("Function copies reference (x2)", "fn foo(x: array[int]) -> unit {}; var y: array[int] = {0, 1}; foo(y); foo(y);", 6),
    ("Function return allows deallocation", "fn f(x: array[array[int]]) -> array[array[int]] { return x; }; f(array[array[int]](10, array[int](10, 10))); f(array[array[int]](10, array[int](10, 10)));", 2*11+100)
  )

  for ((name, program, expectedPeakMemory) <- parallelSimpleMemoryTests) {
    test(name) {
      peakMemoryTest(program, expectedPeakMemory * Value.WORD_SIZE)
    }
    test("Memory Leak Avoidance: " + name) {
      memoryLeakAvoidanceTest(program)
    }
  }

  test("Message passing with recursive function calls") {
    val program="fn dog(x: array[array[int]]) -> unit { if (x[0][1] != 10) { x[0][1] = 10; dog(x); }; }" +
      "fn foobar() -> unit { " +
      "var x: array[array[int]] = {{1, 2}, {3, 4}}; " +
      "if (myX == 0) { send x -> worker 1, 0; recv[array[array[int]]] y <- worker 1, 0; dog(y); dog(y); } " +
      "else { send x -> worker 0, 0; recv[array[array[int]]] y <- worker 0, 0; dog(y); dog(y); }" +
      "}; foobar(); foobar(); foobar(); foobar();"

    val programStream = toStream(program)

    val peakMemoryListeners = Array.fill(2) { new PeakMemoryListener }

    var i = 0
    InterconnectionNetwork.resetLengthWidth(2, 1)
    InterconnectionNetwork.torus()
    InterconnectionNetwork.addGlobalMemoryListener({
      i += 1
      peakMemoryListeners(i-1)
    })

    val currentMemoryListenerArray = Array.tabulate[CurrentMemoryListener](2 * 1) { _ => new CurrentMemoryListener }
    i = 0
    InterconnectionNetwork.addGlobalMemoryListener({
      i += 1;
      currentMemoryListenerArray(i - 1)
    })
    failAfter(1000.millis) {
      InterconnectionNetwork.launchWith(programStream)
    }
    InterconnectionNetwork.printSummaryDiagnostics()

    assert(peakMemoryListeners(0).peak === 24 * Value.WORD_SIZE)
    assert(peakMemoryListeners(1).peak === 24 * Value.WORD_SIZE)
    assert(currentMemoryListenerArray forall (_.currentMemory === 0))
  }

  test("Overriding passed value in messages") {
    val program =
      "fn foobar() -> unit { " +
        "var x: array[int] = {1, 2, 3, 4}; " +
        "if (myX == 0) { " +
          "send x -> worker 1, 0; " +
          "recv x <- worker 1, 0; " +
          "var y: array[int] = {1, 2, 3, 4};" +
          "x[0] = y[0];" +
        "} else { " +
          "send x -> worker 0, 0; " +
          "println(-10);" +
          "recv x <- worker 0, 0; " +
          "var y: array[int] = {1, 2, 3, 4};" +
          "x[0] = y[0];" +
        "}" +
      "}; foobar();"// foobar(); foobar(); "

    val programStream = toStream(program)

    val peakMemoryListeners = Array.fill(2) {
      new PeakMemoryListener
    }

    var i = 0
    InterconnectionNetwork.resetLengthWidth(2, 1)
    InterconnectionNetwork.torus()
    InterconnectionNetwork.addGlobalMemoryListener({
      i += 1
      peakMemoryListeners(i - 1)
    })
    val currentMemoryListenerArray = Array.tabulate[CurrentMemoryListener](2 * 1) { _ => new CurrentMemoryListener }
    i = 0
    InterconnectionNetwork.addGlobalMemoryListener({
      i += 1;
      currentMemoryListenerArray(i - 1)
    })
    failAfter(1000.millis) {
      InterconnectionNetwork.launchWith(programStream)
    }
    InterconnectionNetwork.printSummaryDiagnostics()

    assert(peakMemoryListeners(0).peak === 12 * Value.WORD_SIZE)
    assert(peakMemoryListeners(1).peak === 12 * Value.WORD_SIZE)
    assert(currentMemoryListenerArray forall (_.currentMemory === 0))
  }

  test("Stack allocation IDs are strictly increasing") {
    Value.reset()
    val int1 = new IntegerValue(0)
    val int2 = new IntegerValue(1)
    int1.claimWordId()
    int2.claimWordId()
    assert(int1.wordId === 0)
    assert(int2.wordId === 1)
  }

  test("Array heap allocation IDs are strictly decreasing") {
    Value.reset()
    val arr1 = new ArrayValue(Array.tabulate(10) {i => new IntegerValue(i)})
    arr1.claimWordId()
    assert(arr1.wordId === 0)
    for {i <- arr1.array} {
      assert(i.wordId === MIN_HEAP_WORD - i.asInstanceOf[IntegerValue].int)
    }
  }

  test("Nested array heap allocations are strictly decreasing") {
    Value.reset()
    val arr1 = new ArrayValue(Array.tabulate(10) {i => new ArrayValue(Array.tabulate(10) {j => new IntegerValue(i*10 + j)})})
    arr1.claimWordId()
    assert(arr1.wordId === 0)
    for {(ind, a) <- (0 until 10) zip (arr1.array)} {
      assert(a.wordId === MIN_HEAP_WORD - 2*ind)
      for {i <- a.asInstanceOf[ArrayValue].array} {
        assert(i.wordId === MIN_HEAP_WORD - i.asInstanceOf[IntegerValue].int - 20)
      }
    }
  }
}
