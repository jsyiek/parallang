package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.LatencyContext
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.CacheHierarchy
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.IntegerValue
import uk.ac.cam.crsid.lib.Events.EventListener
import uk.ac.cam.crsid.lib.Events.Listeners.TallyListener
import uk.ac.cam.crsid.lib.Exception.NameDoesNotExistException

import scala.collection.mutable
import scala.concurrent.duration.Duration


class AllocDeallocCounter extends EventListener[MemoryEvent] {


  val tallies: mutable.Map[String, Int] = new mutable.HashMap[String, Int]().withDefaultValue(0)
  override def alert(event: MemoryEvent, timestamp_ps: Long): Unit = event match {
    case AllocationMemoryEvent(bytes, _, _) => tallies("Allocation") += 1
    case DeallocationMemoryEvent(bytes, false) => tallies("Deallocation") += 1
    case _ =>
  }
}

class ExecutionContextTestSuite extends AnyFunSuite {
  private val defaultCacheHierarchy = new CacheHierarchy(0, 100)
  private var i = 0

  def contextSetup(): (ExecutionContext, AllocDeallocCounter) = {
    i = 0
    val l = new LatencyContext
    val e = new ExecutionContext(l, defaultCacheHierarchy)
    val t = new AllocDeallocCounter
    e.subscribe(t)
    (e, t)
  }

  def containsInt(e: ExecutionContext, n: String, v: IntegerValue): Boolean = e.lookup(n) map { _.asInstanceOf[IntegerValue].int == v.int } match {
    case Some(v) => v
    case None => false
  }

  def assertAllocations(c: AllocDeallocCounter, allocations: Int, deallocations: Int): Unit = {
    assert(c.tallies("Allocation") == allocations)
    assert(c.tallies("Deallocation") == deallocations)
  }

  def dummyInt(): IntegerValue = {
    i += 1
    new IntegerValue(i)
  }

  test("Declared element can be looked up context") {
    val (e, _) = contextSetup()
    val v = dummyInt()
    e.declareAndDefine("x", v)
    assert(containsInt(e, "x", v))
  }

  test("Local variable not accessible outside of scope") {
    val (e, _) = contextSetup()
    val v = dummyInt()
    e.withinContext {
      e.declareAndDefine("x", v)
      assert(containsInt(e, "x", v))
    }
    assert(e.lookup("x").isEmpty)
  }

  test("Local variable overrides but not overwrites global variable") {
    val (e, _) = contextSetup()
    val v1 = dummyInt()
    val v2 = dummyInt()
    e.declareAndDefine("x", v1)
    e.withinContext {
      e.declareAndDefine("x", v2)
      assert(containsInt(e, "x", v2))
    }
    assert(containsInt(e, "x", v1))
  }

  test("Updating global in local context updates global") {
    val (e, _) = contextSetup()
    val v1 = dummyInt()
    val v2 = dummyInt()
    e.declareAndDefine("x", v1)
    e.withinContext {
      assert(containsInt(e, "x", v1))
      e.update("x", v2)
      assert(containsInt(e, "x", v2))
    }
    assert(containsInt(e, "x", v2))
  }

  test("Simple local allocation and deallocation") {
    val (e, c) = contextSetup()
    val v = dummyInt()
    assertAllocations(c, 0, 0)
    e.withinContext {
      e.declareAndDefine("x", v)
      assertAllocations(c, 1, 0)
    }
    assertAllocations(c, 1, 0)
  }

  test("Simple global allocation") {
    val (e, c) = contextSetup()
    val v = dummyInt()
    assertAllocations(c, 0, 0)
    e.declareAndDefine("x", v)
    assertAllocations(c, 1, 0)
  }

  test("Simple global and local aliasing allocation and deallocation") {
    val (e, c) = contextSetup()
    val v1 = dummyInt()
    val v2 = dummyInt()
    assertAllocations(c, 0, 0)
    e.declareAndDefine("x", v1)
    assertAllocations(c, 1, 0)
    e.withinContext {
      e.declareAndDefine("x", v2)
      assertAllocations(c, 2, 0)
    }
    assertAllocations(c, 2, 0)
  }

  test("Variables not accessible in different local contexts") {
    val (e, c) = contextSetup()
    val v1 = dummyInt()
    val v2 = dummyInt()
    e.declareAndDefine("x", v1)
    e.withinContext {
      e.declareAndDefine("x", v2)
      e.withinContext {
        assert(containsInt(e, "x", v1))
      }
      assert(containsInt(e, "x", v2))
    }
    assert(containsInt(e, "x", v1))
  }

  test("Variables accessible in different local frames") {
    val (e, c) = contextSetup()
    val v1 = dummyInt()
    val v2 = dummyInt()
    e.declareAndDefine("x", v1)
    e.withinFrame {
      e.declareAndDefine("x", v2)
      e.withinFrame {
        assert(containsInt(e, "x", v2))
      }
      assert(containsInt(e, "x", v2))
    }
    assert(containsInt(e, "x", v1))
  }

  test("Lookup or die throws for non-existent lookups") {
    val (e, c) = contextSetup()
    assertThrows[NameDoesNotExistException](e.lookupOrDie("x"))
  }

  test("Printing context contents doesn't crash") {
    val (e, c) = contextSetup()
    assert(e.getContentsAsString.nonEmpty)
  }
}
