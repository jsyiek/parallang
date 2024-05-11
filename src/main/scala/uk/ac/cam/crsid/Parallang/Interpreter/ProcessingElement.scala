package uk.ac.cam.crsid.Parallang.Interpreter

import uk.ac.cam.crsid.Parallang.Interpreter.CommunicationModel.{InterconnectionNetwork, Message, StrictFIFOQueue}
import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel._
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.CacheModel.CacheHierarchy
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.ExecutionContext
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.Exception.OutOfBoundsArrayAccessException
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values._
import uk.ac.cam.crsid.Parallang.Parser._

import scala.annotation.tailrec
import scala.collection.mutable

class ProcessingElement(val whoAmI: (Int, Int), val rows: Int, val cols: Int, val cacheHierarchy: CacheHierarchy, val doPrint: Boolean=true) {

  val latencyContext: LatencyContext = new LatencyContext()
  val executionContext: ExecutionContext = new ExecutionContext(latencyContext, cacheHierarchy)

  // For simplicity, the external interface communicates on channel `numPE` (and hence we need numPE+1) slots
  // To allow concurrency, we need to export the message passing to separate actors.
  // The Scala Futures API doesn't give us any guarantees on when threads are run, however, so subsequent message
  // sends to the same recipient may be arbitrarily reordered.
  val numPEs: Int = rows*cols
  val messageQueue: Array[StrictFIFOQueue] = Array.fill(numPEs+1) { new StrictFIFOQueue() }
  val sendSeqs: Array[Int] = Array.fill(numPEs+1) { 0 }

  var whyFailed: Option[Throwable] = None

  private var signalReturn: Boolean = false

  private def parseDefnArguments(defnArgs: ASTDefnArguments): List[String] =
    ((0 until defnArgs.jjtGetNumChildren()) foldLeft(List[String]())) {
      case (l: List[String], i) => defnArgs.jjtGetChild(i) match {
        case name: ASTName => name.getName :: l
        case _ => l
      }
    }.reverse

  /*
   * node must be either an ASTName or an ASTIndex
   */
  @tailrec
  private def extractName(node: Node): String = (node: @unchecked) match {
    case name: ASTName => name.getName
    case index: ASTIndex => extractName(index.jjtGetChild(0))
  }

  /*
   * Accessing lvalues in a scalable way is hard, since some are immutable (INTs) and some are mutable (Arrays).
   * This function makes it easy.
   * It returns the result of evaluating the node, and a function that can be passed a value once to
   * update the node.
   *
   * @param node: Node to lvalue access
   * @returns a pair of the value evaluated and a function which, when invoked with a value,
   *          sets the original location to that value
   */
  private def lvalueAcccess(node: Node): (Value, Value => Unit) = node match {
    case nameNode: ASTName =>
      (execute(nameNode), executionContext.update(nameNode.getName, _))
    case indexNode: ASTIndex =>
      initialIndexNodeToModifyFunction(indexNode)
    case _: ASTAttributeAccess =>
      // Implementing lvalue assignment when array indices are present is quite hard.
      // We implement it with a fold that evaluates from left to right the LHS of the assignment operator
      // The fold passes along the following: (v: Value of previous node, f: A function to modify that node)
      // The function `f` is only used on the final node (where v is discarded), but is passed through for simplicity
      // I'll illustrate what the two helper functions do with an example
      //
      // Consider x.y[5].z[3] = 4;
      // `x`, `y`, and `z` are structurally all ASTName nodes. However, `y` and `z` are attributes, and `x` is a variable
      // The initial case needs special handling - in this case, our modify function would need to update the context
      // as opposed to a structure or array. This is all the initialIndexNodeToModifyFunction does.
      //
      // In the general case for subsequent accesses, we need to fold over each and determine their value and what
      // a modify to that node would do, as no node knows if it is the last node. So, we evaluate each node to get
      // the requisite value, and pass along a modify function that allows you to modify it.
      // This function is called at the end.
      ((1 until node.jjtGetNumChildren()) foldLeft initialIndexNodeToModifyFunction (node.jjtGetChild (0))) {
        case ((v, _), i) => nextIndexNodeToModifyFunction (v, node.jjtGetChild (i))
      }
  }

  private def nextIndexNodeToModifyFunction(v: Value, index: Node): (Value, Value => Unit) = {
    index match {
      // Attribute accesses (what this ASTName is) require cache lookups
      case nameNode: ASTName   =>
        (
          v.asInstanceOf[StructValue].getOrDie(nameNode.getName),
          other => v.asInstanceOf[StructValue].putOrDie(nameNode.getName, other)
        )
      case indexNode: ASTIndex =>
        // Type-checking guarantees we have an array here, which is represented as a reference
        // We must pay a cost to resolve the reference.
        // Then, for every index we resolve, we must resolve another array reference which incurs a
        // lookup cost. (We will have paid this already if we're here - looking up in the execution context pays a fee.)

        val attrName = indexNode.jjtGetChild(0).asInstanceOf[ASTName].getName
        val attrValue = v.asInstanceOf[StructValue].getOrDie(attrName)
        val indicesNode = indexNode.jjtGetChild(1).asInstanceOf[ASTIndices]

        // we need to visit the cache to get the starting address of the array
        executionContext.visitCache(attrValue)
        var prevV: ArrayValue = attrValue.asInstanceOf[ArrayValue]
        var currV = attrValue
        var indexValue = new IntegerValue(0)

        for (i <- 0 until indicesNode.jjtGetNumChildren()) {
          // one further attribute lookup for each index
          indexValue = execute(indicesNode.jjtGetChild(i)).asInstanceOf[IntegerValue]
          val valueAtIndex = getAtIndexOrDie(currV.asInstanceOf[ArrayValue], indexValue.int.asInstanceOf[Int], indicesNode.blameToken.get)

          // For it to have type-checked with an index, it must be an array
          prevV = currV.asInstanceOf[ArrayValue]
          currV = valueAtIndex
        }

        (currV, other => { prevV.put(indexValue.int.asInstanceOf[Int], other);  })

    }
  }

  private def initialIndexNodeToModifyFunction(index: Node): (Value, Value => Unit) = {

    index match {
      case nameNode: ASTName => (execute(nameNode), executionContext.update(extractName(nameNode), _))
      case indexNode: ASTIndex =>
        val nameValue = execute(indexNode.jjtGetChild(0))
        val indicesNode = indexNode.jjtGetChild(1).asInstanceOf[ASTIndices]

        var prevV: ArrayValue = nameValue.asInstanceOf[ArrayValue]
        var currV = nameValue
        var indexValue = new IntegerValue(0)

        for (i <- 0 until indicesNode.jjtGetNumChildren()) {
          indexValue = execute(indicesNode.jjtGetChild(i)).asInstanceOf[IntegerValue]

          // We incur a memory penalty for looking up at the index
          val atIndex = getAtIndexOrDie(currV.asInstanceOf[ArrayValue], indexValue.int.asInstanceOf[Int], indicesNode.blameToken.get)

          // For it to have type-checked with an index, it must be an array
          prevV = currV.asInstanceOf[ArrayValue]
          currV = atIndex
        }

        (currV, other => { prevV.put(indexValue.int.asInstanceOf[Int], other) })
    }
  }

  private def getAtIndexOrDie(a: ArrayValue, i: Int, blameToken: Token): Value = a.atIndex(i) match {
    case Some(result) =>
      // need to pay a caching fee for resolving this index
      executionContext.visitCache(result)
      result
    case None => throw new OutOfBoundsArrayAccessException(i, a.len, blameToken)
  }

  private def operatorEvaluation[T <: SimpleNode](node: T): Value = {
    val operatorIterator = 1 until (node.jjtGetNumChildren()-1) by 2
    val valueIterator = 2 until node.jjtGetNumChildren() by 2
    ((operatorIterator zip valueIterator) foldLeft execute(node.jjtGetChild(0))) {
      case (n: Value, (iOp, iVal)) =>
        // Latency for ALU operation
        latencyContext.alertAll(ALUEvent())

        n.handleBinaryOp(
          node.jjtGetChild(iOp).asInstanceOf[ASTOperator].getOperator,
          execute(node.jjtGetChild(iVal))
        )
    }
  }

  def executeStart(node: Node): Unit = try {
    execute(node)
  } catch {
    case e =>
      if (!e.isInstanceOf[InterruptedException]) whyFailed = Some(e)
      InterconnectionNetwork.interruptExecution()
  }

  def execute(node: Node): Value = node match {

    case function: ASTFunction =>
      // Structure:
      // <fn> NAME( DEFNARGS ) -> TYPE { BODY }
      // We need to extract the names from defn args
      val name = function.jjtGetChild(0).asInstanceOf[ASTName].getName
      val namesList = parseDefnArguments(function.jjtGetChild(1).asInstanceOf[ASTDefnArguments])
      val functionBody = function.jjtGetChild(3)
      executionContext.declareAndDefine(name, new FunctionValue(namesList, () => execute(functionBody)))

      // A function definition wouldn't incur any penalty in a compiled language.
      // It would be pre-compiled.

      new UnitValue()

    case call: ASTCall =>
      // Structure
      // NAME ( CALLARGS )

      val funcName = call.jjtGetChild(0).asInstanceOf[ASTName].getName

      // Type-checking guarantees we have a function here.
      val functionValue = executionContext.lookupOrDie(funcName).asInstanceOf[FunctionValue]

      val callArgs = call.jjtGetChild(1)

      // We need to evaluate each function argument before we can proceed
      val functionArgs = (callArgs.jjtGetNumChildren()-1 to 0 by -1 foldLeft List[Value]()) {
        (l, i) => execute(callArgs.jjtGetChild(i)) :: l
      }

      // Endure a penalty from entering a call
      latencyContext.alertAll(BranchEvent())

      // Push a context frame and add all the parameters to the frame
      val result = executionContext.withinContext {
        (functionValue.argNames zip functionArgs) foreach {
          case (n, v) => executionContext.declareAndDefine(n, v)
        }
        functionValue.body()
      }

      // Endure a penalty from leaving a call
      latencyContext.alertAll(BranchEvent())

      // Ensure we reset signalReturn
      signalReturn = false
      result

    case returnNode: ASTReturn =>
      val result = execute(returnNode.jjtGetChild(0))
      signalReturn = true
      result

    case printlnNode: ASTPrintln =>
      // Despite print being computationally expensive, it's not worth modelling in the latency model.
      val value = execute(printlnNode.jjtGetChild(0))
      if (doPrint) println(s"PE $whoAmI: $value")
      new UnitValue()

    case conditional: ASTConditional =>
      for (i <- 0 until conditional.jjtGetNumChildren()) {
        conditional.jjtGetChild(i) match {
          case ifNode: ASTIf =>
            // Amortized branch penalty
            latencyContext.alertAll(BranchEvent())

            // Evaluate condition
            val conditionResult = execute(ifNode.jjtGetChild(0))
            if (conditionResult.asInstanceOf[IntegerValue].int > 0) {
              // Usually a code block will return a unit value
              // However, if it returned, it passes back up the return value
              val result = execute(ifNode.jjtGetChild(1))
              return result
            }
          case elseNode: ASTElse =>
            return execute(elseNode.jjtGetChild(0))
        }
      }
      // It's possible we don't have an else node.
      // In this case, we just return a unit value.
      new UnitValue()

    case forLoop: ASTForLoop =>
      // Structure:
      // <for> <(> EXPRESSION | DECLARATION <;> EXPRESSION <;> EXPRESSION <)> <{> CODE <}>

      executionContext.withinFrame {
        // Initializer
        execute(forLoop.jjtGetChild(0))

        // While the condition is true...
        while (execute(forLoop.jjtGetChild(1)).asInstanceOf[IntegerValue].int > 0) {
          // Execute the body
          val result = execute(forLoop.jjtGetChild(3))

          // For loops have an implicit branch, but I've chosen not to model it with a latency penalty
          // as a branch predictor will usually only be wrong on the very last iteration
          // so the penalty will functionally be absent.

          // If the return signal is true, escape the scope with the return value
          if (signalReturn) return result

          // Otherwise, execute the end of loop expression
          execute(forLoop.jjtGetChild(2))
          latencyContext.alertAll(BranchEvent())
        }
        latencyContext.alertAll(BranchEvent())
      }
      new UnitValue()

    case optExpr: ASTOptExpr =>
      // These expressions are used in for loops to allow for (;;) { ... } syntax
      // They are NOT options.
      // can contain at most one expression
      if (optExpr.jjtGetNumChildren() == 1) {
        execute(optExpr.jjtGetChild(0))
      } else {
        // The vacuous expression is true
        new IntegerValue(1)
      }

    case whileLoop: ASTWhileLoop =>
      // Structure:
      // <while> <(> EXPRESSION <)> <{> CODE <}>

      // While the condition is true...
      while (execute(whileLoop.jjtGetChild(0)).asInstanceOf[IntegerValue].int > 0) {
        // Execute the body
        val result = execute(whileLoop.jjtGetChild(1))

        // If the return signal is true, escape the scope with the return value
        if (signalReturn) return result
        latencyContext.alertAll(BranchEvent())
      }
      latencyContext.alertAll(BranchEvent())
      new UnitValue()

    case send: ASTSend =>
      // Structure:
      // <send> EXPRESSION -> (worker EXPRESSION, EXPRESSION) | broadcast | main
      val outboundMessage = execute(send.jjtGetChild(0))
      val sendTargetNode = send.jjtGetChild(1).asInstanceOf[ASTTarget]
      latencyContext.alertAll(MessageSendEvent(
        InterconnectionNetwork.getLatencyParameterSet.getWirePushTime(outboundMessage),
        InterconnectionNetwork.getLatencyParameterSet.getUncertainWirePushTimes(outboundMessage)
      ))
      sendTargetNode.target match {
        case ASTTarget.Target.WORKER =>
          val (i, j) = (
            execute(sendTargetNode.jjtGetChild(0)).asInstanceOf[IntegerValue].int,
            execute(sendTargetNode.jjtGetChild(1)).asInstanceOf[IntegerValue].int
          )
          InterconnectionNetwork.sendDirect(
            whoAmI,
            (i.asInstanceOf[Int], j.asInstanceOf[Int]),
            outboundMessage,
            send.expectedType,
            latencyContext.getTimestamp,
            latencyContext.getAllTimestamps,
            sendSeqs(InterconnectionNetwork.index(i.asInstanceOf[Int], j.asInstanceOf[Int]))
          )
          sendSeqs(InterconnectionNetwork.index(i.asInstanceOf[Int], j.asInstanceOf[Int])) += 1
        case ASTTarget.Target.BROADCAST =>
          InterconnectionNetwork.broadcast(
            whoAmI,
            outboundMessage,
            send.expectedType,
            latencyContext.getTimestamp,
            latencyContext.getAllTimestamps,
            sendSeqs.indices map {
              i =>
                if (i != InterconnectionNetwork.index(whoAmI._1, whoAmI._2) && i != numPEs) sendSeqs(i) += 1
                sendSeqs(i) - 1
            }
          )
        case ASTTarget.Target.BROADCAST_ROW =>
          InterconnectionNetwork.broadcastRow(
            whoAmI,
            outboundMessage,
            send.expectedType,
            latencyContext.getTimestamp,
            latencyContext.getAllTimestamps,
            (0 until cols) map {
              i =>
                val index = InterconnectionNetwork.index(whoAmI._1, i)
                if (i != whoAmI._2) sendSeqs(index) += 1
                // what we return for ourselves is irrelevant
                // because the interconnection network won't make us send to ourselves.
                sendSeqs(index) - 1
            }
          )
        case ASTTarget.Target.BROADCAST_COL =>
          InterconnectionNetwork.broadcastCol(
            whoAmI,
            outboundMessage,
            send.expectedType,
            latencyContext.getTimestamp,
            latencyContext.getAllTimestamps,
            (0 until rows) map {
              i =>
                val index = InterconnectionNetwork.index(i, whoAmI._2)
                if (i != whoAmI._1) sendSeqs(index) += 1
                sendSeqs(index) - 1
            }
          )
        case ASTTarget.Target.EXTERNAL =>
          InterconnectionNetwork.sendToExternal(
            whoAmI,
            Message(
              outboundMessage,
              send.expectedType,
              latencyContext.getTimestamp,
              latencyContext.getAllTimestamps,
              sendSeqs(numPEs)
            )
          )
          sendSeqs(numPEs) += 1
      }
      new UnitValue()

    case recv: ASTBlockRecv =>
      // Structure:
      // <recv> LVALUE <- (worker EXPRESSION, EXPRESSION) | main
      val f = if (recv.isDecl) {
        executionContext.declareAndDefine(recv.jjtGetChild(1).asInstanceOf[ASTName].getName, _)
      } else {
        // handles the access latency
        val (_, r) = lvalueAcccess(recv.jjtGetChild(0))
        r
      }
      val recvTargetNode = recv.jjtGetChild(if (recv.isDecl) 2 else 1).asInstanceOf[ASTTarget]
      recvTargetNode.target match {
        case ASTTarget.Target.WORKER =>
          val (i, j) = (
            execute(recvTargetNode.jjtGetChild(0)).asInstanceOf[IntegerValue].int,
            execute(recvTargetNode.jjtGetChild(1)).asInstanceOf[IntegerValue].int
          )
          val recvIndex = InterconnectionNetwork.index(i.asInstanceOf[Int], j.asInstanceOf[Int])
          val Message(message, _, avgSendTs_ps, allArrivalTs, _) = messageQueue(recvIndex).getFirst(recv.expectedType)
          latencyContext.alertAll(MessageReadEvent(avgSendTs_ps, allArrivalTs))
          f(message)
        case ASTTarget.Target.EXTERNAL =>
          val Message(message, _, avgSendTs_ps, allArrivalTs, _) = messageQueue(numPEs).getFirst(recv.expectedType)
          latencyContext.alertAll(MessageReadEvent(avgSendTs_ps, allArrivalTs))
          f(message)
        case _ => ???
      }
      new UnitValue()

    case eqNeq: ASTEqNeq =>
      // Structure:
      // EXPRESSION (OP EXPRESSION)*
      operatorEvaluation(eqNeq)

    case logicalOr: ASTLogicalOr =>
      // Structure
      // LOG_AND (<&&> LOG_AND)*
      IntegerValue.liftBoolean(0 until logicalOr.jjtGetNumChildren() exists {
        i =>
          val result = execute(logicalOr.jjtGetChild(i)).asInstanceOf[IntegerValue].int > 0
          latencyContext.alertAll(ALUEvent())
          result
      })

    case logicalAnd: ASTLogicalAnd =>
      // Structure
      // COMPARATOR (<&&> COMPARATOR)*
      IntegerValue.liftBoolean(0 until logicalAnd.jjtGetNumChildren() forall {
        i =>
          val result = execute(logicalAnd.jjtGetChild(i)).asInstanceOf[IntegerValue].int > 0
          latencyContext.alertAll(ALUEvent())
          result
      })

    case mathOp: ASTMathOp =>
      // The structure of an ASTMathOp is
      // EXPRESSION   (OP   EXPRESSION)*
      // Using two index iterators, we can jump two at a time
      // using a fold to correctly evaluate operators at the same precedence level.
      val operatorIterator = 1 until (mathOp.jjtGetNumChildren()-1) by 2
      val valueIterator = 2 until mathOp.jjtGetNumChildren() by 2
      ((operatorIterator zip valueIterator) foldLeft execute(mathOp.jjtGetChild(0))) {
        case (n: Value, (iOp, iVal)) =>
          latencyContext.alertAll(ALUEvent())
          n.handleBinaryOp(
            mathOp.jjtGetChild(iOp).asInstanceOf[ASTOperator].getOperator,
            execute(mathOp.jjtGetChild(iVal))
          )
      }

    case declaration: ASTDeclaration =>
      // Structure:
      // <var> NAME <:> TYPE <=> EXPRESSION
      // Only NAME and EXPRESSION matter here
      val name = declaration.jjtGetChild(0).asInstanceOf[ASTName].getName
      executionContext.declareAndDefine(name, execute(declaration.jjtGetChild(2)))
//      latencyContext.alertAll(AllocationEvent())
      new UnitValue()

    // No action needed for a structure declaration
    case structureDecl: ASTStructureDecl =>
      val name = structureDecl.jjtGetChild(0).asInstanceOf[ASTName].getName
      val defnArgs = structureDecl.jjtGetChild(1)

      val parsedDefnArgs = parseDefnArguments(defnArgs.asInstanceOf[ASTDefnArguments])

      // assume as precondition that any struct value will push its parameters onto the context frame
      executionContext.declareAndDefine(name, new StructInitializerValue(
        parsedDefnArgs,
        new StructValue(name, _)
      ))
      new UnitValue()

    case structValueNode: ASTStructValue =>
      val structGenerator = executionContext.lookupOrDie(
        structValueNode.jjtGetChild(0).asInstanceOf[ASTName].getName
      ).asInstanceOf[StructInitializerValue]


      val callArgs = structValueNode.jjtGetChild(1)
      val map = new mutable.HashMap[String, Value]
      for (
        (name, i) <- structGenerator.argNames zip (0 until callArgs.jjtGetNumChildren())
      ) {
        map(name) = execute(callArgs.jjtGetChild(i))
      }
      structGenerator.body(map)

    case assignment: ASTAssignment =>
      // Structure:
      // ATTRIBUTEACCESS <=> ASSIGNMENT | EXPRESSION
      // evaluate the assignable value
      val rhsValue = execute(assignment.jjtGetChild(1))
      val (_, f) = lvalueAcccess(assignment.jjtGetChild(0))
      f(rhsValue)
      rhsValue

    case attributeAccess: ASTAttributeAccess =>
      val rValue = execute(attributeAccess.jjtGetChild(0))
      ((1 until attributeAccess.jjtGetNumChildren()) foldLeft rValue) {
        (v, i) =>
          val (nextV, _) = nextIndexNodeToModifyFunction(v, attributeAccess.jjtGetChild(i))
          nextV
      }

    case negation: ASTNegation =>
      val integerValue = execute(negation.jjtGetChild(0)).asInstanceOf[IntegerValue]
      integerValue.negate

    case soloExpression: ASTSoloExpression =>
      // Structure:
      // EXPRESSION
      // So just evaluate the child
      execute(soloExpression.jjtGetChild(0))

    // The language is reference based, meaning copying a name
    // just copies the reference unless it's an integer.
    // So, only structure and array values endure an allocation.
    case name: ASTName => executionContext.lookupOrDie(name.getName)
    case number: ASTNumber => new IntegerValue(number.getNumber)
    case _: ASTUnitValue => new UnitValue()
    case _: ASTmyX => new IntegerValue(whoAmI._1)
    case _: ASTmyY => new IntegerValue(whoAmI._2)
    case _: ASTnumRows => new IntegerValue(rows)
    case _: ASTnumCols => new IntegerValue(cols)

    case arrayValue: ASTArrayValue =>
      val callArgs = arrayValue.jjtGetChild(0)
      val array = new Array[Value](callArgs.jjtGetNumChildren())
      (0 until callArgs.jjtGetNumChildren()) foreach {
        i => array(i) = execute(callArgs.jjtGetChild(i))
      }
      // Non-word size attributes require allocations since they are references
      //latencyContext.alertAll(AllocationEvent())
      new ArrayValue(array)

    case indexNode: ASTIndex =>
      // Structure:
      // EXPRESSION ARRAYINDICES

      // We may have a memory lookup here or any number of ALU operations/allocations
      // However, here, we inductively assume these costs have been paid by the sub-nodes
      val array = execute(indexNode.jjtGetChild(0))
      val indices = indexNode.jjtGetChild(1).asInstanceOf[ASTIndices]
      ((0 until indices.jjtGetNumChildren()) foldLeft array) {
        (v, i) =>
          val index = execute(indices.jjtGetChild(i)).asInstanceOf[IntegerValue].int
          // Type-checking guarantees this cast is safe
          val nextArray = v.asInstanceOf[ArrayValue]

          // We now perform an index lookup, which requires a look up to memory
          getAtIndexOrDie(nextArray, index.asInstanceOf[Int], indices.blameToken.get)
      }

    case arrayAutoCreation: ASTArrayAutoCreation =>
      // 0: TYPE, 1: EXPRESSION, 2: VALUE
      val length = execute(arrayAutoCreation.jjtGetChild(1)).asInstanceOf[IntegerValue].int.asInstanceOf[Int]
      val array = new Array[Value](length)
      for (i <- 0 until length) {
        array(i) = execute(arrayAutoCreation.jjtGetChild(2))
      }
      //latencyContext.alertAll(AllocationEvent())
      new ArrayValue(array)

    case len: ASTLen =>
      val body = execute(len.jjtGetChild(0))
      new IntegerValue(body.asInstanceOf[ArrayValue].len)

    case code: ASTCode =>
      executionContext.withinFrame {
        for (i <- 0 until code.jjtGetNumChildren()) {
          val result = execute(code.jjtGetChild(i))
          if (signalReturn) {
            return result
          }
        }
        new UnitValue()
      }

    case start: ASTStart =>
      execute(start.jjtGetChild(0))
  }

}
