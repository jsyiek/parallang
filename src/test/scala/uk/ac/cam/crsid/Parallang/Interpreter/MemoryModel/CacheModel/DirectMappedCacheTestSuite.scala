package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel

import org.scalatest.funsuite.AnyFunSuite

class DirectMappedCacheTestSuite extends AnyFunSuite {

  def testCache: DirectMappedCache = new DirectMappedCache(10, 1, 0)

  test("Sequence of accesses to consecutive lines conflict on the 11th access") {
    val cache = testCache
    for (i <- 0 until 10) {
      assert(cache.cacheLookup(i) === false)
    }
    for (i <- 0 until 10) {
      assert(cache.cacheLookup(i) === true)
    }
    assert(cache.cacheLookup(10) === false)
    assert(cache.cacheLookup(0) === false)
  }

  test("Sequence of conflicting accesses never hits in cache") {
    val cache = testCache
    for (i <- 0 until 10) {
      assert(cache.cacheLookup(i * 10) === false)
    }
    for (i <- 0 until 10) {
      assert(cache.cacheLookup(i * 10) === false)
    }
  }

  test("Duplication works") {
    val cache = testCache
    val dupCache = testCache.duplicate()
    assert(cache.linesPerBlock === dupCache.linesPerBlock)
    assert(cache.numBlocks === dupCache.numBlocks)
    assert(cache.accessPenalty_ps === dupCache.accessPenalty_ps)
  }

}
