package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values

import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.CacheHierarchy
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.Exception.DoubleAllocationException
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.{AllocationMemoryEvent, DeallocationMemoryEvent, ExecutionContext, MemoryEvent}
import uk.ac.cam.crsid.Parallang.Parser.ASTOperator

import java.lang.ThreadLocal
import scala.collection.mutable

object Value {
  val WORD_SIZE: Long = 8
  val MIN_HEAP_WORD: Long = Long.MaxValue - (Long.MaxValue % CacheHierarchy.wordsPerCacheline)
  private val nextStackWordId = new ThreadLocal[Long] { override def initialValue = 0L }
  private val nextHeapWordId = new ThreadLocal[Long] { override def initialValue: Long = MIN_HEAP_WORD }


  // Resets the stack and heap word IDs
  def reset(): Unit = {
    nextStackWordId.set(0)
    nextHeapWordId.set(MIN_HEAP_WORD)
  }

  def newWordId: Long = {
    val toReturn = nextStackWordId.get()
    nextStackWordId.set(toReturn + 1)
    toReturn
  }

  def newMultiWordId(length: Long, useHeap: Boolean): Long = {
    if (useHeap) newMultiWordHeapId(length)
    else newMultiWordStackId(length)
  }

  def newMultiWordStackId(length: Long): Long = {
    var nswid = nextStackWordId.get()
    if (length < 16) {
      if (CacheHierarchy.wordsPerCacheline - nswid % CacheHierarchy.wordsPerCacheline < length) {
        nswid = nswid + CacheHierarchy.wordsPerCacheline - nswid % CacheHierarchy.wordsPerCacheline
      }
    } else {
      nswid = nswid + CacheHierarchy.wordsPerCacheline - nswid % CacheHierarchy.wordsPerCacheline
    }
    nswid += length
    nextStackWordId.set(nswid)
    nswid - length
  }

  def newMultiWordHeapId(length: Long): Long = {
    var nhwid = nextHeapWordId.get()
    if (length < 16) {
      if (nhwid % CacheHierarchy.wordsPerCacheline < length) {
        nhwid -= nhwid % CacheHierarchy.wordsPerCacheline
      }
    } else {
      nhwid -= nhwid % CacheHierarchy.wordsPerCacheline
    }
    nhwid -= length
    nextHeapWordId.set(nhwid)
    nhwid + length
  }
}


// Used to ensure that recursive classes (e.g., arrays, structs) only allocate
// if their children are unallocated and not if just copied.
// it's a way for a child to pay for its parent block allocation time.
// reasoning (stack vs heap allocation):
  // base integers do not need to be heap allocated, for example
  // but the integers inside of an array need to be.
  // the main reason we care is that stack-allocation is
  // free in comparison to heap allocation.
// reasoning (block allocation):
  // some data (like arrays) can be allocated as a block in a single allocation of a parent structure/array
  // to which it is a constituent
// As an optimization, allow multi-dimensional arrays to be allocated as one block and not pay many reallocations
class AllocationCheck(var hasAllocated: Boolean, var parentIsArray: Boolean = false) {}

abstract class Value(var wordId: Long = -1) {

   val references: mutable.HashSet[Object] = mutable.HashSet[Object]()

  // What execution context this value is stored in.
  var executionContext: Option[ExecutionContext] = None

  // To track memory usage, we use a form of cycle-oblivious reference counting.
  // Any reference link passes down the ExecutionContext it is within.
  // A naive approach using the observer pattern where lower references alert higher
  // references breaks down due to Option types, which allow cyclic references.
  // This is a known weakness of reference counts, but reference counting is used by
  // major languages nonetheless with the caveat to programmers: don't lose track of
  // cyclic references!

  /*
   * Makes this value a shallow copy of another.
   * C++ terms: it's the assignment operator.
   */
  def makeShallowCopyOf(v: Value): Unit = ()

  /*
   * Creates a shallow copy of this.
   * C++ terms: it's the assignment operator
   */
  def createShallowCopy(): Value

  /*
   * Tells the value to claim a word ID if it does not already have one
   * -1 => The "no word assigned" ID.
   */
  def claimWordId(mandatedID: Long = -1): Unit = {
    if (this.wordId == -1L) {
      if (mandatedID != -1L) {
        this.wordId = mandatedID
      } else {
        this.wordId = Value.newMultiWordId(this.sizeInWords(), useHeap = false)
      }
    }
  }

  def linkReference(o: Object, e: Option[ExecutionContext]): Unit = {
    references.add(o)
    e foreach (exCont => declareContext(exCont))
  }

  def unlinkReference(o: Object): Unit = {
    references -= o

    if (references.isEmpty) {
      // only need to report a deallocation if the value is heap allocated.
      // the value is heap allocated iff our parent is a value
      if (sizeInBytes() > 0) alertContext(getDeallocationEvent)
      onDeallocation()
      executionContext = None
    }
  }

  def getDeallocationEvent: DeallocationMemoryEvent = DeallocationMemoryEvent(sizeInBytes(), hasNoHeapDependents = false)

  def declareContext(e: ExecutionContext, allocationCell: Option[AllocationCheck]=None): Unit = {
    executionContext match {
      case None =>
        executionContext = Some(e)

        // There's no point declaring an empty allocation - e.g. a function
        // The latter parameter controls whether or not to report a time penalty for the allocation -
        // we only want to report this once for each chunk of data that can be block allocated.
        // Strictly-stack-allocated values are basically free
        // false => report penalty
        // true => don't report penalty
        if (sizeInBytes() > 0) {
          // we should only report a time penalty if we're asked to by our parent.
          alertContext(AllocationMemoryEvent(sizeInBytes(), allocationCell.forall(_.hasAllocated)))
          allocationCell.foreach(_.hasAllocated = true)
        }
        declareContextToChildren(e, allocationCell)
      case Some(e2) if e2 ne e =>
        println(e2, e)
        throw new DoubleAllocationException(s"$this was already bound to $e2, but was asked to bound to $e!")
      case _ => ()
    }
  }

  def onDeallocation(): Unit

  def declareContextToChildren(e: ExecutionContext, allocationCell: Option[AllocationCheck] = None): Unit

  def alertContext(event: MemoryEvent): Unit = executionContext foreach { _.alertAll(event) }

  def handleBinaryOp(operator: ASTOperator.Operator, other: Value): Value

  /*
   * To simulate the distributed memory model, passing a message requires it to be
   * cloned. We cannot simply transfer the reference as otherwise we are sharing memory!
   */
  def deepCopy(): Value

  def sizeInBytes(): Long = this.sizeInWords() * Value.WORD_SIZE

  def sizeInWords(): Long

  def totalSizeInBytes(): Long = sizeInBytes()

  def abandonWordId(): Unit = {
    this.wordId = -1
  }
}