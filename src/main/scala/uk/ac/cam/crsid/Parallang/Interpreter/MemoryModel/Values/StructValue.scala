package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values

import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.ExecutionContext
import uk.ac.cam.crsid.Parallang.Parser.ASTOperator.Operator
import uk.ac.cam.crsid.lib.Exception.NameDoesNotExistException

import scala.collection.mutable

class StructValue(name: String, var attrs: mutable.HashMap[String, Value]) extends Value {

  override def handleBinaryOp(operator: Operator, other: Value): Value = operator match {
    case Operator.EQ  => IntegerValue.liftBoolean(this == other)
    case Operator.NEQ => IntegerValue.liftBoolean(this != other)
  }

  def getOrDie(attr: String): Value = attrs.get(attr) match {
    case None    => throw new NameDoesNotExistException(s"Could not find $attr as an attribute of $name.")
    case Some(v) => v
  }

  def putOrDie(name: String, value: Value): Unit = {
    attrs.get(name)
    attrs(name) = value
  }

  override def deepCopy(): Value = {
    val dupAttrs: mutable.HashMap[String, Value] = mutable.HashMap()
    attrs foreach {
      case (k, v) => dupAttrs.put(k, v.deepCopy())
    }
    new StructValue(
      name, dupAttrs
    )
  }

  override def toString: String = {
    val attrStr = attrs.mkString(",")
    s"$name($attrStr)"
  }

  override def onDeallocation(): Unit = attrs.values foreach { _.unlinkReference(this) }

  override def declareContextToChildren(e: ExecutionContext, a: Option[AllocationCheck]=None): Unit = {
    val allocationCheck = new AllocationCheck(false)
    attrs.values foreach {
      _.declareContext(e, Some(allocationCheck))
    }
  }

  // A structure is a reference to a tuple of its children,*
  // so it doesn't use anymore memory than the sum
  // of its children, and its children will report
  // their allocations.
  // Therefore, just report the reference
  override def sizeInWords(): Long = 1

  override def makeShallowCopyOf(v: Value): Unit = {
    val v2 = v.asInstanceOf[StructValue]
    if (v2.attrs != attrs) {
      attrs.values foreach { _.unlinkReference(this) }
      attrs = v2.attrs
      attrs.values foreach { _.linkReference(this, executionContext) }
    }
  }

  override def createShallowCopy(): Value = new StructValue(name, attrs)

  override def claimWordId(mandatedID: Long): Unit = {
    attrs.values foreach {
      _.linkReference(this, executionContext)
    }
    super.claimWordId(mandatedID)
    if (attrs.nonEmpty) {
      // Attributes are allocated or not as a unit.
      val (_, value) = attrs.head
      if (value.wordId != -1) {
        val childIds = attrs.values map {v => Value.newMultiWordId(v.sizeInWords(), useHeap = true)}
        (childIds zip attrs.values) foreach { case (i, v) => v.claimWordId(i) }
      }
    }
  }

  override def totalSizeInBytes(): Long = sizeInBytes() + attrs.values.foldLeft(0L)((acc, v) => acc + v.totalSizeInBytes())

  override def abandonWordId(): Unit = {
    this.wordId = -1
    attrs.values foreach {_.abandonWordId()}
  }
}
