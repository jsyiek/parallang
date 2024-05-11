package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime

class TreePLRUTestSuite extends AnyFunSuite {

  test("Simple Tree PLRU sequence") {
    val tplu = new TreePLRUFullyAssociativeCache(4, 1, 0 /* s */)
    tplu.cacheLookup(0)
    tplu.cacheLookup(1)
    tplu.cacheLookup(2)
    tplu.cacheLookup(3)
    tplu.cacheLookup(4)
    assert(!tplu.cacheLookup(0))
  }

  test("Simple Tree PLRU sequence (2)") {
    val tplu = new TreePLRUFullyAssociativeCache(4, 1, 0)
    tplu.cacheLookup(3)
    tplu.cacheLookup(2)
    tplu.cacheLookup(1)
    tplu.cacheLookup(0)
    tplu.cacheLookup(4)
    assert(!tplu.cacheLookup(3))
  }

  test("Simple Tree PLRU sequence (3)") {
    val tplu = new TreePLRUFullyAssociativeCache(4, 1, 0)
    tplu.cacheLookup(0)
    tplu.cacheLookup(1)
    tplu.cacheLookup(0)
    tplu.cacheLookup(2)
    tplu.cacheLookup(0)
    tplu.cacheLookup(3)
    assert(!tplu.cacheLookup(1))
  }

  test("Complex Tree PLRU sequence") {
    val tplu = new TreePLRUFullyAssociativeCache(8, 1, 0)
    tplu.cacheLookup(0)
    tplu.cacheLookup(1)
    tplu.cacheLookup(5)
    tplu.cacheLookup(2)
    tplu.cacheLookup(3)
    tplu.cacheLookup(3)
    tplu.cacheLookup(3)
    tplu.cacheLookup(2)
    tplu.cacheLookup(5)
    tplu.cacheLookup(3)
    tplu.cacheLookup(3)
    tplu.cacheLookup(2)
    tplu.cacheLookup(4)
    tplu.cacheLookup(6)
    tplu.cacheLookup(3)
    tplu.cacheLookup(7)
    tplu.cacheLookup(4)
    tplu.cacheLookup(3)
    tplu.cacheLookup(3)
    tplu.cacheLookup(4)
    tplu.cacheLookup(4)
    assert(tplu.cacheLookup(0))
    assert(tplu.cacheLookup(1))
    assert(!tplu.cacheLookup(2))
    assert(tplu.cacheLookup(3))
    assert(!tplu.cacheLookup(4))
    assert(tplu.cacheLookup(5))
    assert(tplu.cacheLookup(6))
    assert(!tplu.cacheLookup(7))
  }

  test("Complex Set-Assoc Tree PLRU sequence") {
    val tplu = new TreePLRUSetAssociativeCache(6, 1, 0, 3)
    tplu.cacheLookup(0)
    tplu.cacheLookup(1)
    tplu.cacheLookup(5)
    tplu.cacheLookup(2)
    tplu.cacheLookup(3)
    tplu.cacheLookup(3)
    tplu.cacheLookup(3)
    tplu.cacheLookup(2)
    tplu.cacheLookup(5)
    tplu.cacheLookup(3)
    tplu.cacheLookup(3)
    tplu.cacheLookup(2)
    tplu.cacheLookup(4)
    tplu.cacheLookup(6)
    tplu.cacheLookup(3)
    tplu.cacheLookup(7)
    tplu.cacheLookup(4)
    tplu.cacheLookup(3)
    tplu.cacheLookup(3)
    tplu.cacheLookup(4)
    tplu.cacheLookup(4)
    assert(!tplu.cacheLookup(0))
    assert(!tplu.cacheLookup(1))
    assert(tplu.cacheLookup(2))
    assert(tplu.cacheLookup(3))
    assert(!tplu.cacheLookup(4))
    assert(tplu.cacheLookup(5))
    assert(!tplu.cacheLookup(6))
    assert(!tplu.cacheLookup(7))
  }

  test ("Duplicate works (full associativity)") {
    val v = new TreePLRUFullyAssociativeCache(1, 2, 3)
    val v2 = v.duplicate()
    assert(v2.numBlocks === v.numBlocks)
    assert(v2.linesPerBlock === v.linesPerBlock)
    assert(v2.accessPenalty_ps === v.accessPenalty_ps)
  }

  test("Duplicate works (set associativity)") {
    val v = new TreePLRUSetAssociativeCache(4, 3, 2, 1)
    val v2 = v.duplicate()
    assert(v2.numBlocks === v.numBlocks)
    assert(v2.linesPerBlock === v.linesPerBlock)
    assert(v2.accessPenalty_ps === v.accessPenalty_ps)
  }

}
