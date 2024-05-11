package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values

import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.CacheHierarchy
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.{AllocationMemoryEvent, DeallocationMemoryEvent, ExecutionContext}
import uk.ac.cam.crsid.Parallang.Parser.ASTOperator.Operator
import uk.ac.cam.crsid.Parallang.TypeChecking.{ArrayType, IntType, Type}

import scala.collection.mutable

object ArrayValue {

  def fromScalaArray(a: Array[_]): (Value, Type) = {
    var t: Type = null
    (new ArrayValue(
      a map {
        case aElem: Array[_] =>
          val (v, typed) = fromScalaArray(aElem)
          t = typed
          v
        case iElem: Long =>
          t = IntType()
          new IntegerValue(iElem)
        case e =>
          throw new IllegalArgumentException(s"Expected a possibly nested array of longs, but instead " +
            s"found a (possibly nested) array of ${e.getClass.getName}")
      }
    ), ArrayType(t))
  }

  def matrixToScalaArray(a: ArrayValue): Array[Array[Long]] = {
    a.array map ( _.asInstanceOf[ArrayValue].array map (_.asInstanceOf[IntegerValue].int))
  }
}

private class ReferenceCount(val array: Array[Value], val aliasers: mutable.HashSet[ArrayValue]) {
  var boundToArray = false
  def bind(a: ArrayValue, e: Option[ExecutionContext]): Unit = {
    if (!boundToArray) {
      array foreach { _.linkReference(this, e) }
      boundToArray = true
    }
    aliasers += a
  }

  def unbind(a: ArrayValue): Unit = {
    val wereTheyBound = aliasers.contains(a)
    aliasers -= a
    if (aliasers.isEmpty && boundToArray) {
      boundToArray = false
      array foreach { _.unlinkReference(this) }
    }
  }
}

class ArrayValue(var array: Array[Value]) extends Value {

  // the number of pointers to the array held by this ArrayValue.
  // we need to track this so we don't double free the array
  // when we have heap aliasing between different stack refs
  // see createShallowCopy
  private var referenceCount = new ReferenceCount(array, new mutable.HashSet[ArrayValue])

  // On construction, link this array to its children
//  array foreach { _.linkReference(this, executionContext) }

  override def handleBinaryOp(operator: Operator, other: Value): Value = operator match {
    case Operator.EQ  => if (this == other) new IntegerValue(1) else new IntegerValue(0)
    case Operator.NEQ => if (this != other) new IntegerValue(1) else new IntegerValue(0)
  }

  def atIndex(i: Int): Option[Value] = {
    array.lift(i)
  }

  def put(i: Int, v: Value): Boolean =
    if (i >= array.length)
      false
    else {
      if (array(i) != v) array(i).makeShallowCopyOf(v)
      true
    }

  def len: Int = array.length

  override def makeShallowCopyOf(v: Value): Unit = {
    val v2 = v.asInstanceOf[ArrayValue]
    if (v2.array != array) {
      referenceCount.unbind(this)
      // report deallocation of orphaned child array
      if (referenceCount.aliasers.isEmpty && array.nonEmpty) {
        alertContext(DeallocationMemoryEvent(0, hasNoHeapDependents = false))
      }

      // if v2 is an anonymous value (e.g., an array literal)
      // then we need to pay an allocation event for the underlying array we
      // are assigning ourselves to.
      if (v2.executionContext.isEmpty) {
        alertContext(AllocationMemoryEvent(0, forceReport = true))
      }

      array = v2.array
      this.referenceCount = v2.referenceCount
      referenceCount.bind(this, executionContext)
      if (v2.array.nonEmpty && v2.array(0).wordId == -1L) claimWordId()
    }
  }

  override def createShallowCopy(): Value = {
    val a = new ArrayValue(array)
    a.referenceCount = referenceCount
    a
  }

  override def deepCopy(): Value = {
    new ArrayValue(
      Array.tabulate(array.length) {
        i => array(i).deepCopy()
      }
    )
  }

  override def claimWordId(mandatedID: Long = -1): Unit = {
    if (this.wordId == -1) referenceCount.bind(this, executionContext)

    // The ArrayValue is unique in that it needs two word IDs (length + ref to start of arr).
    // To ensure both are in the same cacheline (so that accessing one
    // will bring the other into the cache also), we need to ensure
    // arrays don't have the last word in a cacheline.
    // This way, we don't need to handle the word ID for the two separately.
    referenceCount.bind(this, executionContext)
    super.claimWordId(mandatedID)
    // the array will have a word ID or not as a unit for all elements
    if (array.nonEmpty && array(0).wordId == -1) {
      val childIDs = array map { v => Value.newMultiWordId(v.sizeInWords(), useHeap = true) }
      (0 until array.length) foreach { i => array(i).claimWordId(childIDs(i)) }
    }

  }

  override def toString: String = array.mkString("{", ", ", "}")

  override def onDeallocation(): Unit = {
    referenceCount.unbind(this)
  }

  override def declareContextToChildren(e: ExecutionContext, allocationCell: Option[AllocationCheck] = None): Unit = {
    // allow multi-dimensional arrays to only pay for allocation once, USE INSTEAD OF "FALSE": allocationCell.exists(a => a.hasAllocated && a.parentIsArray)
    val allocationCheck = new AllocationCheck(false, parentIsArray=true)
    array foreach {
      _.declareContext(e, Some(allocationCheck))
    }
  }

  // An array is stored in its parent variable as a reference as its parent
  // doesn't know the array's size (they are variable length).
  // An array is a contiguous allocation of elements.
  // Therefore, the array's size allocation will be fully accounted for by
  // the children when its children report their sizes.
  // Only the length of the array (which is accessed in O(1)) must be stored.
  override def sizeInWords(): Long = 2

  override def totalSizeInBytes(): Long = sizeInBytes() + array.foldLeft(0L)((acc, v) => acc + v.totalSizeInBytes())

  override def getDeallocationEvent: DeallocationMemoryEvent = {
    // array has no heap dependents iif its empty
    if (this.wordId != -1) referenceCount.unbind(this)
    DeallocationMemoryEvent(sizeInBytes(), hasNoHeapDependents = array.isEmpty || referenceCount.aliasers.nonEmpty)
  }


  override def abandonWordId(): Unit = {
    this.wordId = -1
    array foreach {
      _.abandonWordId()
    }
  }
}
