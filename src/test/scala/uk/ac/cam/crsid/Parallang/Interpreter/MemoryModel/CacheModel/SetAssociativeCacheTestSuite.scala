package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.ReplacementStrategy.{CachelineReplacementStrategy, LeastRecentlyUsed}

class SetAssociativeCacheTestSuite extends AnyFunSuite {

  class SetAssociativeTestCache(numBlocks: Long,
                                linesPerBlock: Long,
                                accessPenalty_ps: Long,
                                associativity: Int)
    extends SetAssociativeCache(numBlocks, linesPerBlock, accessPenalty_ps, associativity, new LeastRecentlyUsed) {
    def failsFor(cachelineId: Long): Assertion = assert(cacheLookup(cachelineId) === false, s"Lookup for $cachelineId succeeded when it should have failed")

    def succeedsFor(cachelineId: Long): Assertion = assert(cacheLookup(cachelineId) === true, s"Lookup for $cachelineId failed when it should have succeeded")
  }

  def testCache(numBlocks: Long = 4,
                linesPerBlock: Long = 1,
                accessPenalty_ps: Long = 1,
                associativity: Int = 2) =
    new SetAssociativeTestCache(numBlocks, linesPerBlock, accessPenalty_ps, associativity)

  test("Cyclic pattern that overfills the cache by one") {
    val cache = testCache()

    cache failsFor 0
    cache failsFor 1
    cache failsFor 2
    cache failsFor 3
    cache failsFor 4

    cache failsFor 0
    cache succeedsFor 0
    cache succeedsFor 1
    cache failsFor 2
    cache succeedsFor 3
    cache failsFor 4
  }

  test("Replacement policy defines which line to evict") {
    // test using LRU
    val cache = testCache(numBlocks = 4, linesPerBlock = 1)

    // prime cache
    cache failsFor 0
    cache failsFor 1
    cache failsFor 2
    cache failsFor 3

    // simulate an access pattern
    cache succeedsFor 2
    cache succeedsFor 0
    cache succeedsFor 2
    cache succeedsFor 3
    cache succeedsFor 1
    cache succeedsFor 0

    // eviction required
    cache failsFor 4

    // check it evicted 2
    cache succeedsFor 2
  }

  test("Duplicate works") {
    val v = new SetAssociativeCache(4, 3, 2, 1, new LeastRecentlyUsed)
    val v2 = v.duplicate()
    assert(v2.numBlocks === v.numBlocks)
    assert(v2.linesPerBlock === v.linesPerBlock)
    assert(v2.accessPenalty_ps === v.accessPenalty_ps)
  }

}
