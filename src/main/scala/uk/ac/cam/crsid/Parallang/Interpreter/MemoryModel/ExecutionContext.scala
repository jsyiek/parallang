package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel

import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.{AllocationEvent, CachedMemoryLookupEvent, DeallocationEvent, LatencyContext}
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.CacheHierarchy
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.Value
import uk.ac.cam.crsid.lib.Events.{EventGenerator, EventListener}

import scala.collection.mutable
import scala.concurrent.duration.Duration

class ExecutionContext(latencyContext: LatencyContext, cacheHierarchy: CacheHierarchy) extends EventGenerator[MemoryEvent]{
  private var localContexts = List[OwnedMemoryContext]()
  private val globalContext = new OwnedMemoryContext(this)

  globalContext.pushFrame()

  private def pushContext(): Unit = {
    localContexts = new OwnedMemoryContext(this) :: localContexts
    localContexts.head.pushFrame()
  }
  private def popContext(): Unit = localContexts = localContexts match {
    case _ :: tls => tls
    case empty => empty
  }

  private def pushFrame(): Unit = localContexts match {
    case hd :: _ => hd.pushFrame()
    case _ => globalContext.pushFrame()
  }
  private def popFrame(): Unit = localContexts match {
    case hd :: _ => hd.popFrame()
    case _ => globalContext.popFrame()
  }

  /*
   * Used to declare a variable and define it. Requires a memory access to write into the new variable, and one
   * to access and visit the old
   */
  def declareAndDefine(name: String, value: Value): Unit = {
    val toWrite = value.createShallowCopy()
    visitCache(value)

    toWrite.claimWordId()
    visitCache(toWrite)

    localContexts.headOption.map(_.put(name, toWrite)) match {
      case None => globalContext.put(name, toWrite)
      case Some(_) => ()
    }
  }

  def update(name: String, value: Value): Unit = {
    // we need to visit cache for the thing we're copying from...
    visitCache(value)
    // ...and for the thing we're copying to.
    // assume as a precondition that lookup calls visitCache
    lookup(name)
    localContexts.find(_.contains(name)) match {
      case Some(c) => c.update(name, value)
      case None => globalContext.update(name, value)
    }
  }

  def lookup(name: String): Option[Value] = {
    val result = localContexts.headOption.flatMap(_.lookup(name)) match {
      case None => globalContext.lookup(name)
      case o => o
    }
    result match {
      case Some(v) => visitCache(v)
      case _ => ()
    }
    result
  }

  def lookupOrDie(name: String): Value = {
    val result = localContexts.headOption.flatMap(_.lookup(name)) match {
      case None => globalContext.lookupOrDie(name)
      case Some(v) => v
    }
    visitCache(result)
    result
  }

  def withinFrame[A](body: => A): A = {
    pushFrame()
    try {
      body
    } finally {
      popFrame()
    }
  }

  def withinContext[A](body: => A): A = {
    pushContext()
    try {
      body
    } finally {
      localContexts.head.deallocateAllFrames()
      popContext()
    }
  }

  override def alertAll(event: MemoryEvent): Unit = {
    event match {
      case AllocationMemoryEvent(bytes, parentReported, forceReport) =>
        if (!parentReported) latencyContext.alertAll(AllocationEvent())
        if (bytes > 0 || forceReport) super.alertAll(event)
      case DeallocationMemoryEvent(_, false) =>
        latencyContext.alertAll(DeallocationEvent())
        super.alertAll(event)
      case DeallocationMemoryEvent(_, true) =>
        /* don't report a temporal penalty for deallocation of an item with no heap dependents,
           but do report it to memory listeners */
        super.alertAll(event)
    }
  }

  def visitCache(value: Value): Unit = if (value.wordId != -1) latencyContext.alertAll(cacheHierarchy.lookup(value.wordId))

  override protected def currTimestamp(): Long = latencyContext.getTimestamp

  def getContentsAsString: String = "Local Contexts:\n" + localContexts + "\nGlobal Context:\n" + globalContext

  override def toString: String =
    s"ExecutionContext" + (if (listeners.nonEmpty) "\n\t\t" else "") + listeners.mkString("\n\t\t")
}
