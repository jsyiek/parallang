package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.ReplacementStrategy

import scala.collection.mutable

trait CachelineReplacementStrategy {
  def evictCacheline(contents: mutable.Set[Long]): Unit
  def informOfAccess(blockNumber: Long): Unit
}