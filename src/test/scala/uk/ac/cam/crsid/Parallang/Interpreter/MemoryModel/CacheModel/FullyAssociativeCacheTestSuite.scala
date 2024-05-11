package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.ReplacementStrategy.{CachelineReplacementStrategy, LeastRecentlyUsed, NotLastUsed}

import scala.concurrent.duration.Duration

class FullyAssociativeCacheTestSuite extends AnyFunSuite {

  class FullyAssociativeTestCache(numBlocks: Long,
                                  linesPerBlock: Long,
                                  accessPenalty_ps: Long,
                                  cachelineReplacementStrategy: CachelineReplacementStrategy)
    extends FullyAssociativeCache(numBlocks, linesPerBlock, accessPenalty_ps, cachelineReplacementStrategy)
  {
    def failsFor(cachelineId: Long): Assertion = assert(!cacheLookup(cachelineId))
    def succeedsFor(cachelineId: Long): Assertion = assert(cacheLookup(cachelineId))
  }

  def testCache(numBlocks: Long=4,
                linesPerBlock: Long=2,
                accessPenalty_ps: Long=1) =
    new FullyAssociativeTestCache(numBlocks, linesPerBlock, accessPenalty_ps, new LeastRecentlyUsed())


  test("Simple repeated access pattern") {
    val cache = testCache()

    cache failsFor 10

    cache succeedsFor 10
    cache succeedsFor 10
    cache succeedsFor 10
    cache succeedsFor 10
  }

  test("Cyclic pattern that perfectly fills cache") {
    val cache = testCache(numBlocks=4, linesPerBlock=1)

    cache failsFor 0
    cache failsFor 1
    cache failsFor 2
    cache failsFor 3

    cache succeedsFor 0
    cache succeedsFor 1
    cache succeedsFor 2
    cache succeedsFor 3
  }

  test("Cyclic pattern that overfills the cache by one") {
    val cache = testCache(numBlocks=4, linesPerBlock=1)

    cache failsFor 0
    cache failsFor 1
    cache failsFor 2
    cache failsFor 3
    cache failsFor 4

    cache failsFor 0
    cache failsFor 1
    cache failsFor 2
    cache failsFor 3
    cache failsFor 4
  }

  test("Replacement policy defines which line to evict") {
    // test using LRU
    val cache = testCache(numBlocks=4, linesPerBlock=1)

    // prime cache
    cache failsFor 0
    cache failsFor 1
    cache failsFor 2
    cache failsFor 3

    // simulate an access pattern
    // initial eviction queue: [0, 1, 2, 3]
    cache succeedsFor 2     // [0, 1, 3, 2]
    cache succeedsFor 0     // [1, 3, 2, 0]
    cache succeedsFor 2     // [1, 3, 0, 2]
    cache succeedsFor 3     // [1, 0, 2, 3]
    cache succeedsFor 1     // [0, 2, 3, 1]
    cache succeedsFor 0     // [2, 3, 1, 0]

    // eviction required
    cache failsFor 4        // [3, 1, 0, 4]

    // check it evicted 2
    cache failsFor 2
  }

  test("Duplicates successfully") {
    val v = new FullyAssociativeCache(1, 2, 3, new LeastRecentlyUsed)
    val v2 = v.duplicate()
    assert(v2.numBlocks === v.numBlocks)
    assert(v2.linesPerBlock === v.linesPerBlock)
    assert(v2.accessPenalty_ps === v.accessPenalty_ps)
  }

  test("Not last used policy works") {
    val v = new FullyAssociativeCache(5, 1, accessPenalty_ps = 10, new NotLastUsed)
    v.cacheLookup(0)
    v.cacheLookup(1)
    v.cacheLookup(2)
    v.cacheLookup(3)
    v.cacheLookup(4)
    v.cacheLookup(5)
    assert(v.cacheLookup(4) === true)
    assert(v.cacheLookup(5) === true)
    v.cacheLookup(2)
    assert(v.cacheLookup(5) === true)
    assert(v.cacheLookup(2) === true)
    v.cacheLookup(1)
    assert(v.cacheLookup(2) === true)
    assert(v.cacheLookup(1) === true)
  }

}
