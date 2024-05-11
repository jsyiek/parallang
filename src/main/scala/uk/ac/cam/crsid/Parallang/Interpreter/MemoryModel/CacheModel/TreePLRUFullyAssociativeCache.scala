package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.{Duration, DurationInt}

class TreePLRUFullyAssociativeCache(numBlocks: Long, linesPerBlock: Long, accessPenalty_ps:Long)
  extends Cache(numBlocks: Long, linesPerBlock: Long, accessPenalty_ps: Long) {

  assert(numBlocks > 0 && (numBlocks & (numBlocks - 1)) == 0, "Number of blocks must be a power of 2 for TreePLRU")

  val tree: Array[Boolean] = Array.fill(numBlocks.asInstanceOf[Int]-1)(false)
  val array: Array[Long] = Array.fill(numBlocks.asInstanceOf[Int])(-1)

  var numBlocksWriteable = numBlocks
  var log2 = -1;
  while (numBlocksWriteable > 0) {
    log2 += 1;
    numBlocksWriteable = numBlocksWriteable >>> 1
  }

  val indices: mutable.HashMap[Long, Long] = new mutable.HashMap[Long, Long]

  override def cacheLookup(id: Long): Boolean = {
    indices.get(cachelineIdToBlockId(id)) match {
      case None =>
        var index = 0
        var treeTraversalIndex = 0
        for (_ <- 0 until log2) {
          index <<= 1;
          tree(treeTraversalIndex) = !tree(treeTraversalIndex)
          if (!tree(treeTraversalIndex)) {
            index += 1
            treeTraversalIndex <<= 1
            treeTraversalIndex += 2
          } else {
            treeTraversalIndex <<= 1
            treeTraversalIndex += 1
          }
        }
        indices.remove(array(index))
        indices(cachelineIdToBlockId(id)) = index
        array(index) = cachelineIdToBlockId(id)
        false
      case Some(index) =>
        var reverseIndex = reverse(index)
        var treeTraversalIndex = 0
        for (_ <- 0 until log2) {
          val bitIs1: Boolean = (reverseIndex & 1L) == 1L
          if (bitIs1 == tree(treeTraversalIndex)) {
            tree(treeTraversalIndex) = !tree(treeTraversalIndex)
          }
          treeTraversalIndex += (if (bitIs1) 2 else 1)
          reverseIndex >>>= 1;
        }
        true
    }
  }

  @tailrec
  final def reverse(inp: Long, out: Long=0): Long = if (inp == 0) out else reverse(inp >>> 1, out << 1 + inp & 1)

  override def duplicate(): Cache = new TreePLRUFullyAssociativeCache(
    numBlocks, linesPerBlock, accessPenalty_ps
  )
}
