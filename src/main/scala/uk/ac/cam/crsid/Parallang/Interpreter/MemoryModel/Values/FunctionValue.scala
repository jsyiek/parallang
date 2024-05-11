package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values

import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.ExecutionContext
import uk.ac.cam.crsid.Parallang.Parser.ASTOperator

class FunctionValue(val argNames: List[String], val body: () => Value) extends Value {
  override def handleBinaryOp(operator: ASTOperator.Operator, other: Value): Value =
    throw new UnsupportedOperationException("Functions don't support binary operations.")

  override def deepCopy(): Value = throw new RuntimeException("A FunctionValue should NOT be passed as a message!")

  def call(): Value = body()

  // A function value doesn't use any heap memory.
  override def onDeallocation(): Unit = ()

  override def declareContextToChildren(e: ExecutionContext, a: Option[AllocationCheck]=None): Unit = ()

  override def sizeInWords(): Long = 0

  // This should NEVER claim a memory location
  override def claimWordId(mandatedID: Long): Unit = ()

  // It doesn't matter if this is shallow copied as it is immutable, so just return itself.
  override def createShallowCopy(): Value = this

}
