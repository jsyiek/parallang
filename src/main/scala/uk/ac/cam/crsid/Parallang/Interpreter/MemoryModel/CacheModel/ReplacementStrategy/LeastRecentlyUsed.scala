package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.ReplacementStrategy

import uk.ac.cam.crsid.lib.Collection.PushFallPriorityQueue

import scala.collection.mutable

class LeastRecentlyUsed extends CachelineReplacementStrategy {

  private val accessHistory: PushFallPriorityQueue[Long] = new PushFallPriorityQueue[Long]()

  override def evictCacheline(contents: mutable.Set[Long]): Unit =
    accessHistory.dequeue() foreach { contents.remove }

  override def informOfAccess(blockNumber: Long): Unit = accessHistory.push(blockNumber)
}