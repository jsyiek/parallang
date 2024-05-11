package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values

import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.ExecutionContext
import uk.ac.cam.crsid.Parallang.Parser.ASTOperator

class UnitValue extends Value {
  override def handleBinaryOp(operator: ASTOperator.Operator, other: Value): Value =
    throw new UnsupportedOperationException("You cannot perform an operation on a unit value!")

  override def deepCopy(): Value = new UnitValue()

  // No further actions required when deallocating as the super deals with notifying about the size dealloc
  override def onDeallocation(): Unit = ()

  override def declareContextToChildren(e: ExecutionContext, a: Option[AllocationCheck]=None): Unit = ()

  override def sizeInWords(): Long = 1

  override def createShallowCopy(): Value = new UnitValue
}
