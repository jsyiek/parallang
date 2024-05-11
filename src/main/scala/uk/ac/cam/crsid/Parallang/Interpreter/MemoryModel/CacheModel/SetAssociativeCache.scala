package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel

import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.ReplacementStrategy.CachelineReplacementStrategy

import scala.concurrent.duration.Duration

class SetAssociativeCache(numBlocks: Long, linesPerBlock: Long, accessPenalty_ps: Long, associativity: Int, cachelineReplacementStrategy: => CachelineReplacementStrategy)
  extends Cache(numBlocks, linesPerBlock, accessPenalty_ps) {

  assert(numBlocks % associativity == 0, "A set associative cache must have numBlocks as a multiple of associativity")

  val contents = Array.fill[FullyAssociativeCache](associativity)(new FullyAssociativeCache(numBlocks/associativity, linesPerBlock, accessPenalty_ps, cachelineReplacementStrategy))

  override def cacheLookup(id: Long): Boolean = {
    val blockId = cachelineIdToBlockId(id)
    val index = (blockId % associativity).asInstanceOf[Int]
    contents(index).cacheLookup(id)
  }

  override def duplicate(): Cache = new SetAssociativeCache(
    numBlocks, linesPerBlock, accessPenalty_ps, associativity, cachelineReplacementStrategy
  )
}
