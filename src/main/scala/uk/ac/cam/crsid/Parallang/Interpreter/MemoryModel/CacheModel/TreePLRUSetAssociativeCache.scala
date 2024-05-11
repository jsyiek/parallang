package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel

import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.ReplacementStrategy.CachelineReplacementStrategy

import scala.concurrent.duration.Duration

class TreePLRUSetAssociativeCache(numBlocks: Long, linesPerBlock: Long, accessPenalty_ps: Long, associativity: Int)
  extends Cache(numBlocks, linesPerBlock, accessPenalty_ps) {

  assert(numBlocks % associativity == 0, "A set associative cache must have numBlocks as a multiple of associativity")

  val contents = Array.fill[TreePLRUFullyAssociativeCache](associativity)(new TreePLRUFullyAssociativeCache(numBlocks/associativity, linesPerBlock, accessPenalty_ps))

  override def cacheLookup(id: Long): Boolean = {
    val blockId = cachelineIdToBlockId(id)
    val index = (blockId % associativity).asInstanceOf[Int]
    contents(index).cacheLookup(id)
  }

  override def duplicate(): Cache = new TreePLRUSetAssociativeCache(
    numBlocks, linesPerBlock, accessPenalty_ps, associativity
  )
}
