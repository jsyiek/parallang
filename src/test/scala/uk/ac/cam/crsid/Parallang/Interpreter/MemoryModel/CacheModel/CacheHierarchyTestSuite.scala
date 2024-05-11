package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.CachedMemoryLookupEvent
import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.TimingLong._
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.ReplacementStrategy.LeastRecentlyUsed
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.IntegerValue

import scala.concurrent.duration.Duration
import scala.language.{implicitConversions, postfixOps}
import scala.math.pow

class CacheHierarchyTestSuite extends AnyFunSuite {

  def nLevelCache(n: Int, levelAccessLatency: Long=1.picoseconds): CacheHierarchy = {
    val cacheHierarchy = new CacheHierarchy(n=n, levelAccessLatency);
    for (i <- 0 until n) {
      cacheHierarchy.setLevel(i+1, new FullyAssociativeCache(
        numBlocks = pow(2, i).toLong,
        linesPerBlock = 1,
        accessPenalty_ps = levelAccessLatency,
        cacheReplacementPolicy = new LeastRecentlyUsed()
      ))
    }
    cacheHierarchy
  }

  class CacheOutcome(outcome: CachedMemoryLookupEvent) {
    /*
     * Helper class that allows idiomatic assertion of cache outcomes
     */
    def andMisses: Assertion = {
      assert(outcome.levelOfHit === -1)
    }

    def andHitsAt(n: Int): Assertion = {
      assert(outcome.levelOfHit === n)
    }

    def takes(d: Long): CacheOutcome = {
      assert(outcome.latency_ps === d)
      this
    }

  }

  class CacheMonitor(levels: Int, levelAccessLatency: Long=1.picoseconds) {
    /*
     * Helper class that allows idiomatic description of cache test behavior
     */
    val cacheHierarchy: CacheHierarchy = nLevelCache(levels, levelAccessLatency)

    def cachelineToWord(cachelineId: Long): Long= cachelineId * CacheHierarchy.wordsPerCacheline

    def lookupFor(cachelineId: Long): CacheOutcome =
      new CacheOutcome(cacheHierarchy.lookup(cachelineToWord(cachelineId)))
  }


  test("Empty cache hierarchy goes to memory") {
    val hierarchy = new CacheMonitor(levels=5)
    // 6.ns = 5*cache fails + 1*memory lookup
    hierarchy lookupFor 0 takes 6.picoseconds andMisses;
    hierarchy lookupFor 0 takes 1.picoseconds andHitsAt 1
  }

  test("Forced miss in L1 cache goes to L2") {
    val hierarchy = new CacheMonitor(levels=5)
    hierarchy lookupFor 0 takes 6.picoseconds andMisses;
    hierarchy lookupFor 1 takes 6.picoseconds andMisses;
    hierarchy lookupFor 1 takes 1.picoseconds andHitsAt 1
    hierarchy lookupFor 0 takes 2.picoseconds andHitsAt 2
    hierarchy lookupFor 1 takes 2.picoseconds andHitsAt 2
  }

  test("Forced miss in L2 cache goes to L3") {
    val hierarchy = new CacheMonitor(levels=5)
    hierarchy lookupFor 0 takes 6.picoseconds andMisses;
    hierarchy lookupFor 1 takes 6.picoseconds andMisses;
    hierarchy lookupFor 2 takes 6.picoseconds andMisses;

    hierarchy lookupFor 0 takes 3.picoseconds andHitsAt 3
    hierarchy lookupFor 1 takes 3.picoseconds andHitsAt 3
    hierarchy lookupFor 0 takes 2.picoseconds andHitsAt 2
    hierarchy lookupFor 2 takes 3.picoseconds andHitsAt 3
    hierarchy lookupFor 2 takes 1.picoseconds andHitsAt 1
  }

  test("Complex cache hierarchy lookup sequence") {
    val hierarchy = new CacheMonitor(levels=5)

    // L1: [_], L2: [_, _], L3: [_, _, _, _], L4: [...]
    hierarchy lookupFor 1 takes 6.picoseconds andMisses;
    hierarchy lookupFor 2 takes 6.picoseconds andMisses;
    hierarchy lookupFor 3 takes 6.picoseconds andMisses;
    hierarchy lookupFor 4 takes 6.picoseconds andMisses;
    hierarchy lookupFor 5 takes 6.picoseconds andMisses;

    // L1: [5], L2: [4, 3], L3: [5, 4, 3, 2], L4: [1...5]
    hierarchy lookupFor 2 takes 3.picoseconds andHitsAt 3

    // L1: [2], L2: [2, 4], L3: [2, 5, 4, 3], L4: [1...5]
    hierarchy lookupFor 1 takes 4.picoseconds andHitsAt 4

    // L1: [1], L2: [1, 2], L3: [1, 2, 5, 4], L4: [1...5]
    hierarchy lookupFor 1 takes 1.picoseconds andHitsAt 1

    // L1: [1], L2: [1, 2], L3: [1, 2, 5, 4], L4: [1...5]
    hierarchy lookupFor 2 takes 2.picoseconds andHitsAt 2

    // L1: [2], L2: [2, 1], L3: [1, 2, 5, 4], L4: [1...5]
    hierarchy lookupFor 5 takes 3.picoseconds andHitsAt 3

    // L1: [5], L2: [5, 2], L3: [5, 1, 2, 4], L4: [1...5]
    hierarchy lookupFor 3 takes 4.picoseconds andHitsAt 4

    // L1: [3], L2: [3, 5], L3: [3, 5, 1, 2], L4: [1...5]
  }

  test("Cannot submit invalid cache") {
    val cacheHierarchy = new CacheHierarchy(n = 2, 100)
    cacheHierarchy.setLevel(1, new DirectMappedCache(2, 2, 2))
    assertThrows[InvalidCacheException](cacheHierarchy.setLevel(2, new DirectMappedCache(5, 5, 5)))
  }

  test("Cannot submit cache at higher level than exists") {
    val cacheHierarchy = new CacheHierarchy(n = 2, 100)
    assertThrows[InvalidCacheException](cacheHierarchy.setLevel(10, new DirectMappedCache(2, 2, 2)))
  }

  test("Validation fails for undefined cache") {
    val cacheHierarchy = new CacheHierarchy(2, 1000)
    assertThrows[InvalidCacheException](cacheHierarchy.validate())
  }

  test("Cache lookup for -1 ID does not incur penalty") {
    val cacheHierarchy = new CacheHierarchy(1, 100)
    cacheHierarchy.setLevel(1, new DirectMappedCache(2, 2, 2))
    assert(cacheHierarchy.lookup(-1).latency_ps === 0)
  }

  test("Can duplicate cache hierarchy") {
    val c = new CacheHierarchy(2, 100)
    c.setLevel(1, new DirectMappedCache(2, 2, 2))
    c.setLevel(2, new DirectMappedCache(2, 2, 2))
    val c2 = c.duplicate()
    assert(c2.memoryAccessLatency === c.memoryAccessLatency)
    assert(c2.lookup(10) === c.lookup(10))
    assert(c2.lookup(100) === c.lookup(100))
    assert(c2.lookup(1000) === c.lookup(1000))
    assert(c2.lookup(10) === c.lookup(10))
    assert(c2.lookup(100) === c.lookup(100))
  }

  test("Cache hierarchy can lookup values") {
    val i1 = new IntegerValue(1)
    val i2 = new IntegerValue(1)
    val i3 = new IntegerValue(1)
    val i4 = new IntegerValue(1)
    val i5 = new IntegerValue(1)
    i1.wordId = 0
    i2.wordId = 8
    i3.wordId = 16
    i4.wordId = 24
    i5.wordId = 32
    val c = new CacheHierarchy(1, 100)
    c.setLevel(1, new DirectMappedCache(2, 1, 2))
    assert(c.lookup(i1).levelOfHit === -1)
    assert(c.lookup(i2).levelOfHit === 1)
    assert(c.lookup(i1).levelOfHit === 1)
    assert(c.lookup(i1).levelOfHit === 1)
    assert(c.lookup(i3).levelOfHit === -1)
    assert(c.lookup(i4).levelOfHit === 1)
    assert(c.lookup(i1).levelOfHit === 1)
    assert(c.lookup(i2).levelOfHit === 1)
    assert(c.lookup(i3).levelOfHit === 1)
    assert(c.lookup(i5).levelOfHit === -1)
    assert(c.lookup(i1).levelOfHit === -1)
    assert(c.lookup(i5).levelOfHit === -1)
    assert(c.lookup(i5).levelOfHit === 1)
  }

}
