package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.ReplacementStrategy

import scala.collection.mutable

class NotLastUsed extends CachelineReplacementStrategy {

  var lastUsed: Long = -1
  override def evictCacheline(contents: mutable.Set[Long]): Unit = {
    var n: Int = 0
    do {
      n = util.Random.nextInt(contents.size)
    } while (contents.iterator.drop(n).next == lastUsed)
    contents.remove(contents.iterator.drop(n).next)
  }

  override def informOfAccess(blockNumber: Long): Unit = lastUsed = blockNumber
}