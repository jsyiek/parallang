package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values

import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.{DeallocationMemoryEvent, ExecutionContext}
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.IntegerValue.{infinity, negativeInfinity}
import uk.ac.cam.crsid.Parallang.Parser.ASTOperator.Operator

class IntegerValue(var int: Long) extends Value {

  override def handleBinaryOp(operator: Operator, other: Value): Value = {
    // We have performed type checking and can safely cast other to an IntegerValue
    val operation = mathOp(other.asInstanceOf[IntegerValue]) _
    operator match {
      case Operator.PLUS  => operation((a, b) =>
        if (a == infinity || b == infinity)
          infinity
        else if (a == negativeInfinity || b == negativeInfinity)
          negativeInfinity
        else a+b
      )
      case Operator.SUB   => operation((a, b) =>
        if (a == infinity || b == negativeInfinity)
          infinity
        else if (b == infinity || a == negativeInfinity)
          negativeInfinity
        else a - b
      )
      case Operator.TIMES => operation((a, b) =>
        if (a == 0 || b == 0)
          0
        else if (a == infinity && b == infinity || a == negativeInfinity && b == negativeInfinity)
          infinity
        else if (a == infinity || b == infinity)
          infinity * (1 * (if (a < 0) -1 else 1) * (if (b < 0) -1 else 1))
        else if (a == negativeInfinity || b == negativeInfinity)
          infinity * (1 * (if (a < 0) -1 else 1) * (if (b < 0) -1 else 1))
        else a * b
      )
      case Operator.DIV   => operation(_/_)
      case Operator.EQ    => operation((a, b) => if (a == b) 1 else 0)
      case Operator.NEQ   => operation((a, b) => if (a != b) 1 else 0)
      case Operator.GREATER_EQUAL => operation((a, b) => if (a >= b) 1 else 0)
      case Operator.LESS_EQUAL    => operation((a, b) => if (a <= b) 1 else 0)
      case Operator.GREATER       => operation((a, b) => if (a > b) 1 else 0)
      case Operator.LESS          => operation((a, b) => if (a < b) 1 else 0)
      // NEED TO THROW EXCEPTION IF NOT SUPPORTED
    }
  }

  override def getDeallocationEvent: DeallocationMemoryEvent = DeallocationMemoryEvent(sizeInBytes(), hasNoHeapDependents = true)

  def negate: IntegerValue = new IntegerValue(-int)

  def mathOp(v: IntegerValue)(op: (Long, Long) => Long) = new IntegerValue(op(this.int, v.int))

  override def deepCopy() = new IntegerValue(int)

  override def toString: String = if (int == infinity) "inf" else if (int == negativeInfinity) "-inf" else int.toString

  override def onDeallocation(): Unit = ()

  override def declareContextToChildren(e: ExecutionContext, a: Option[AllocationCheck]=None): Unit = ()

  // The integer is a classic one-word object
  override def sizeInWords(): Long = 1

  override def makeShallowCopyOf(v: Value): Unit = {
    val v2 = v.asInstanceOf[IntegerValue]
    int = v2.int
  }

  override def createShallowCopy(): Value = new IntegerValue(int)
}

object IntegerValue {
  val infinity: Long = Long.MaxValue
  val negativeInfinity: Long = -Long.MaxValue

  def liftBoolean(b: Boolean): IntegerValue = if (b) new IntegerValue(1) else new IntegerValue(0)
}