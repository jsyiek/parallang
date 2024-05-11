package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel

abstract class Cache(val numBlocks: Long, val linesPerBlock: Long, val accessPenalty_ps: Long) {
  /*
   * Returns true if the requested word ID is in the cache and false otherwise.
   * If it is not in the cache, it adds the block containing the word to the cache.
   */
  def cacheLookup(id: Long): Boolean

  /*
   * Returns the block ID to which the cacheline belongs.
   */
  def cachelineIdToBlockId(cachelineId: Long): Long = cachelineId/linesPerBlock

  // Returns a duplicate of the cache
  // Used to use the same model CacheHierarchy (and hence Caches) for all PEs
  def duplicate(): Cache
}
