package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel

import scala.collection.mutable
import scala.concurrent.duration.Duration

class DirectMappedCache(numBlocks: Long, linesPerBlock: Long, accessPenalty_ps: Long)
  extends Cache(numBlocks, linesPerBlock, accessPenalty_ps) {

  val contents: Array[Long] = Array.fill[Long](numBlocks.asInstanceOf[Int])(Long.MaxValue)

  override def cacheLookup(id: Long): Boolean = {
    val blockId = cachelineIdToBlockId(id)
    val index: Int = (blockId % numBlocks).asInstanceOf[Int]
    if (contents(index) == blockId) {
      return true
    }
    contents(index) = blockId
    false
  }

  override def duplicate(): Cache = new DirectMappedCache(numBlocks, linesPerBlock, accessPenalty_ps)
}
