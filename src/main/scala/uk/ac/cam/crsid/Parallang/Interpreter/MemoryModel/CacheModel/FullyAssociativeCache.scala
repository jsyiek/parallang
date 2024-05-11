package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel

import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.ReplacementStrategy.CachelineReplacementStrategy

import scala.collection.mutable
import scala.concurrent.duration.Duration

class FullyAssociativeCache(numBlocks: Long,
                            linesPerBlock: Long,
                            accessPenalty_ps: Long,
                            cacheReplacementPolicy: CachelineReplacementStrategy)
  extends Cache(numBlocks, linesPerBlock, accessPenalty_ps) {

  private val contents = new mutable.HashSet[Long]()

  override def cacheLookup(cachelineId: Long): Boolean = {

    val blockId = cachelineIdToBlockId(cachelineId)

    val isInCache = contents.contains(blockId)

    if (isInCache) {
      // ensure that the cache replacement policy is informed before we return.
      cacheReplacementPolicy.informOfAccess(blockId)
      return true
    }

    // check if cache is full
    if (contents.size >= numBlocks) {
      cacheReplacementPolicy.evictCacheline(contents)
    }

    // ensure that the cache replacement policy is informed before we return.
    cacheReplacementPolicy.informOfAccess(blockId)
    contents.add(blockId)

    false
  }

  override def duplicate(): Cache = new FullyAssociativeCache(
    numBlocks,
    linesPerBlock,
    accessPenalty_ps,
    cacheReplacementPolicy)
}
