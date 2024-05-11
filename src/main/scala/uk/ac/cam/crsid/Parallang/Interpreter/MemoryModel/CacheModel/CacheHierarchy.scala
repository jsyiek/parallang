package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel

import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.CachedMemoryLookupEvent
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.CacheHierarchy.wordsPerCacheline
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.Value

import scala.concurrent.duration.{Duration, DurationInt}
import scala.util.control.Breaks
import scala.util.control.Breaks.{break, breakable}

object CacheHierarchy {
  val wordsPerCacheline: Long = 16
}

/*
 * Models a NINE cache hierarchy with n levels.
 * Serves only the purpose of calculating latency.
 */
class CacheHierarchy(n: Int, val memoryAccessLatency: Long) {

  private val hierarchy: Array[Cache] = new Array(n)

  def duplicate(): CacheHierarchy = {
    val cloned = new CacheHierarchy(n, memoryAccessLatency)
    for ((c, i) <- hierarchy.zipWithIndex) {
      cloned.hierarchy(i) = c.duplicate()
    }
    cloned
  }

  def setLevel(inpLevel: Int, cache: Cache): CacheHierarchy = {
    val level = inpLevel-1
    if (level < 0 || level >= n) {
      throw new InvalidCacheException(s"There are only $n levels, so a cache at level ${level+1} cannot be fitted.", n+1)
    }

    // The model assumes that each cache block can be fully contained in a parent cache
    // To this end, we need to complain noisily if a child cache block can't be fully contained in their
    // parent
    if ((level < n-1) && (hierarchy(level+1) != null)
        && (cache.linesPerBlock < hierarchy(level+1).linesPerBlock
            || hierarchy(level+1).linesPerBlock % cache.linesPerBlock != 0
      )
    ) {
      throw new InvalidCacheException(s"Cache blocks at level ${level+1} (${cache.linesPerBlock}) aren't fully contained " +
        s"by the parent's blocks at level ${level+2} (${cache.linesPerBlock})", level+1)
    }

    // Likewise, we need to check the child.
    if ((level > 0) && (hierarchy(level-1) != null
        && (
          hierarchy(level-1).linesPerBlock < cache.linesPerBlock
          ||  cache.linesPerBlock % hierarchy(level-1).linesPerBlock != 0
        ))
    ) {
      throw new InvalidCacheException(s"Cache blocks at level ${level+1} (${cache.linesPerBlock}) don't fully contain " +
        s"their child's blocks at level ${level} (${cache.linesPerBlock}).", level+1)
    }

    hierarchy(level) = cache
    this
  }

  def validate(): Unit = {
    for ((c, i) <- hierarchy.zipWithIndex) {
      if (c == null) {
        throw new InvalidCacheException(s"No cache defined for level ${i+1}", i+1)
      }
    }
  }

  def wordIdToCachelineId(wordId: Long): Long = wordId/wordsPerCacheline

  def lookup(value: Value): CachedMemoryLookupEvent = lookup(value.wordId)
  def lookup(wordId: Long): CachedMemoryLookupEvent = {
    if (wordId == -1) {
      return CachedMemoryLookupEvent(0 /* seconds */, -1)
    }

    val cachelineId = wordIdToCachelineId(wordId)
    var hitLevel = -1
    var cacheLatencyCost: Long = 0
    breakable {
      for ((c, i) <- hierarchy.zipWithIndex) {
        cacheLatencyCost += c.accessPenalty_ps
        if (c.cacheLookup(cachelineId)) {
          hitLevel = i+1
          break
        }
      }
    }
    CachedMemoryLookupEvent(cacheLatencyCost + (if (hitLevel == -1) memoryAccessLatency else 0L /* ps */ ), hitLevel)
  }

}
