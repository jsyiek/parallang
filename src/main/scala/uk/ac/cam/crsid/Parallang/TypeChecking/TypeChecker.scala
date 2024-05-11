package uk.ac.cam.crsid.Parallang.TypeChecking

import uk.ac.cam.crsid.Parallang.Parser._
import uk.ac.cam.crsid.Parallang.TypeChecking.Exception._
import uk.ac.cam.crsid.lib.Collection.Context
import uk.ac.cam.crsid.lib.Exception.BlameTokenException

import scala.util.{Failure, Success, Try}

object TypeChecker {

  /**
   * Type checks a syntax tree.
   * @param node The root of the syntax tree to check.
   * @return Success(Unit) if the type check was successful. Failure(e) where e is a BlameTokenException
   *         in the event of failure, which contains the specifics of the failure.
   */
  def evaluate(node: ASTStart): Try[Unit] = {
    try {
      evaluate(node, NoReturnAnyType(), new Context[Type]())
      Success(())
    } catch {
      // We only want to handle explicit parse exceptions.
      // Any others should propagate upward.
      case blameTokenException: BlameTokenException => Failure(blameTokenException)
      case e => throw e
    }
  }

  /**
    Type checks a syntax tree.
    @param node the syntax tree to evaluate
    @return whether all control paths through the node contained a return statement
   */
  private def evaluate(node: Node,
                       requiresReturnType: Type,
                       typeContext: Context[Type]): Boolean = node match {

    case start: ASTStart =>
      typeContext.pushFrame()
      evaluate(start.jjtGetChild(0), NoReturnAnyType(), typeContext)

    // `forall` short circuits, so need to use `count`
    case code: ASTCode => (0 until code.jjtGetNumChildren() count {
      i => evaluate(code.jjtGetChild(i), requiresReturnType, typeContext)
    }) > 0

    case function: ASTFunction =>
      val nameNode = function.jjtGetChild(0).asInstanceOf[ASTName]
      val functionName = nameNode.getName

      requireOutOfScope(functionName, typeContext, nameNode.blameToken.get)

      val defnArguments = Type.defnArgumentsToList(
        function.jjtGetChild(1).asInstanceOf[ASTDefnArguments],
        typeContext,
        function.blameToken.get
      )
      val returnType = Type.from(
        function.jjtGetChild(2).asInstanceOf[ASTType],
        typeContext,
        function.blameToken.get
      )

      // Double declarations in same scope are illegal
      typeContext.lookupTopContext(functionName) match {
        case Some(_) => throw new RedeclarationException(functionName, function.blameToken.get)
        case _ => ()
      }

      // Push function name before evaluating function type to allow self-recursive functions
      typeContext.put(
        name=functionName,
        value=FunctionType(
          defnArguments,
          returnType
        ))

      // push new local context
      typeContext.pushFrame()

      // Push the function's parameters as variables into the new local context
      typeContext.putMultiple(
        Type.defnArgumentsToLinkedHashMap(
          function.jjtGetChild(1).asInstanceOf[ASTDefnArguments],
          typeContext,
          function.blameToken.get
        )
      )
      (returnType, evaluate(function.jjtGetChild(3), returnType, typeContext)) match {
        // allow UnitType not to have a return type down every branch
        case (_: UnitType, false) | (_, true) => ()
        case (_, false) => throw new MissingReturnPathException(functionName, function.blameToken.get)
      }

      // After exiting function, pop its local context
      typeContext.popFrame()

      // The function node doesn't return, so we return false
      false

    case structureDecl: ASTStructureDecl =>
      val nameNode = structureDecl.jjtGetChild(0).asInstanceOf[ASTName]
      val structureName = nameNode.getName

      requireOutOfScope(structureName, typeContext, nameNode.blameToken.get)

      val attrs = Type.defnArgumentsToLinkedHashMap(
        structureDecl.jjtGetChild(1).asInstanceOf[ASTDefnArguments],
        typeContext,
        structureDecl.blameToken.get
      )
      val structType = StructType(structureName, attrs, isInitializer = true)

      requireOutOfScope(structureName, typeContext, structureDecl.blameToken.get)

      typeContext.put(
        name=structureName,
        value=structType
      )

      // the structure declaration node doesn't return anything
      false

    case structValue: ASTStructValue =>
      val nameNode = structValue.jjtGetChild(0).asInstanceOf[ASTName]
      val structName = nameNode.getName

      val structType = typeContext.lookup(structName) match {
        case None => throw new UndefinedStructException(structName, nameNode.blameToken.get)
        case Some(StructType(name, attrs, true)) => StructType(name, attrs, false)
        case Some(StructType(name, attrs, false)) => throw new NotCallableException(nameNode.getName, StructType(name, attrs), nameNode.blameToken.get)
        case Some(v) => throw new NotAStructException(v, nameNode.blameToken.get)
      }

      requireMatch(requiresReturnType, structType, nameNode.blameToken.get)

      val callArguments = structValue.jjtGetChild(1)
      val numArgsForStruct = structType.attrs.values.size
      if (callArguments.jjtGetNumChildren() != numArgsForStruct) {
        throw new IncorrectArgumentNumberException(
          structName,
          numArgsForStruct,
          callArguments.jjtGetNumChildren(),
          nameNode.blameToken.get
        )
      }

      (0 until callArguments.jjtGetNumChildren()) zip structType.attrs.values foreach {
        case (i, requiredType) => evaluate(callArguments.jjtGetChild(i), requiredType, typeContext)
      }

      false


    case call: ASTCall =>
      val functionName = call.jjtGetChild(0).asInstanceOf[ASTName].getName
      // Function must exist to call it.
      val functionType = typeContext.lookup(functionName) match {
        case Some(t) => t
        case _ => throw new UndefinedVariableException(
          functionName,
          call.blameToken.get
        )
      }

      functionType match {
        case FunctionType(argsType, returnType) =>
          // Require the return type to be correct
          requireMatch(requiresReturnType, returnType, call.blameToken.get)
          val callArguments = call.jjtGetChild(1)
          if (callArguments.jjtGetNumChildren() != argsType.length) {
            throw new IncorrectArgumentNumberException(
              functionName,
              argsType.length,
              callArguments.jjtGetNumChildren(),
              call.blameToken.get
            )
          }

          (0 until callArguments.jjtGetNumChildren()) zip argsType foreach {
            case (i, aType) => evaluate(callArguments.jjtGetChild(i), aType, typeContext)
          }
        case _ => throw new NotCallableException(functionName, functionType, call.blameToken.get)
      }
      false

    case returnNode: ASTReturn =>
      requiresReturnType match {
        case NoReturnAnyType() => throw new ToplevelReturnException()
        case _ => ()
      }
      evaluate(returnNode.jjtGetChild(0), requiresReturnType, typeContext)
      true

    case conditional: ASTConditional =>
      // Conditional returns down every control path if every sub-node of it does (if/elif/else)
      // and it contains an else branch.
      // If an else is present, the last branch is always an else.
      (0 until conditional.jjtGetNumChildren() count {
        i => evaluate(conditional.jjtGetChild(i), requiresReturnType, typeContext)
      }) == conditional.jjtGetNumChildren() && conditional.jjtGetChild(conditional.jjtGetNumChildren() - 1).isInstanceOf[ASTElse]

    case ifNode: ASTIf =>
      // Check the condition, require it to be an integer
      evaluate(ifNode.jjtGetChild(0), IntType(), typeContext)

      typeContext.pushFrame()
      // Check the body. The ifNode returns down its control path if its body does.
      val result = evaluate(ifNode.jjtGetChild(1), requiresReturnType, typeContext)
      typeContext.popFrame()
      result

    case elseNode: ASTElse =>
      // Similarly to the ifNode, the elseNode returns down its control path if its body does.
      typeContext.pushFrame()
      val result = evaluate(elseNode.jjtGetChild(0), requiresReturnType, typeContext)
      typeContext.popFrame()
      result

    case forNode: ASTForLoop =>
      // <for> "(" (DECLARATION | EXPRESSION) "," EXPRESSION "," EXPRESSION ")" "{" CODE "}"
      // node 0 can introduce names to the context, so the context needs to be pushed at the start, but otherwise has
      //        no type requirement.
      // node 1 must be an integer as it is the termination condition
      // node 2 has no type requirement
      // node 3's type only matters for the return value
      typeContext.withinFrame {
        evaluate(forNode.jjtGetChild(0), MimicType(), typeContext)
        evaluate(forNode.jjtGetChild(1), IntType(), typeContext)
        evaluate(forNode.jjtGetChild(2), MimicType(), typeContext)
        evaluate(forNode.jjtGetChild(3), requiresReturnType, typeContext)
      }
      // it's not possible to guarantee a for loop will be entered, so we cannot assume it is guaranteed to return
      false

    case optExpr: ASTOptExpr =>
      if (optExpr.jjtGetNumChildren() == 1) {
        evaluate(optExpr.jjtGetChild(0), requiresReturnType, typeContext)
      } else {
        false
      }

    case whileNode: ASTWhileLoop =>
      // <while> ( EXPRESSION ) { CODE }
      typeContext.withinFrame {
        evaluate(whileNode.jjtGetChild(0), IntType(), typeContext)
        evaluate(whileNode.jjtGetChild(1), MimicType(), typeContext)
      }
      // it's not possible to guarantee a while loop will be entered, so we cannot assume it is guaranteed to return
      false

    // SoloExpression exists solely to allow top-level expressions to not be bound by the return type.
    // They can only appear in the language in the context a statement could appear in (and thus cannot
    // be sub-nodes of statements, which have stricter typing rules).
    case soloExpression: ASTSoloExpression =>
      evaluate(soloExpression.jjtGetChild(0), MimicType(), typeContext)
      false

    case printlnNode: ASTPrintln =>
      // We don't care what's in a println as long as it is consistent
      evaluate(printlnNode.jjtGetChild(0), AnyValueType(), typeContext)
      false

    case lenNode: ASTLen =>
      requireMatch(requiresReturnType, IntType(), lenNode.blameToken.get)
      // Whatever is inside of a `len` must be an array of anything
      evaluate(lenNode.jjtGetChild(0), ArrayType(MimicType()), typeContext)
      false

    case getNode: ASTGet =>
      val mimicType = MimicType()
      // need to mimic the option type in the child node
      evaluate(getNode.jjtGetChild(0), mimicType, typeContext)

      // The mimicType should never return None when passed to a
      // child node. We're perfectly happy with a match error in this case.
      (mimicType.mimickedType: @unchecked) match {
        case Some(OptionType(_)) => ()
        case Some(other) => throw new NotAnOptionException(
          other,
          getNode.blameToken.get
        )
      }
      false

    case index: ASTIndex =>
      // we need to get the array type of the object being indexed
      val mimicType = MimicType()
      evaluate(index.jjtGetChild(0), mimicType, typeContext)

      // This is guaranteed to be fully instantiated
      val lhsType = mimicType.mimickedType.get

      val indicesNode = index.jjtGetChild(1).asInstanceOf[ASTIndices]
      val finalIndexType =
        (0 until indicesNode.jjtGetNumChildren() foldRight lhsType) { (i: Int, t: Type) =>
          t match {
            case ArrayType(of) =>
              evaluate(indicesNode.jjtGetChild(i), IntType(), typeContext)
              of
            case _ =>
              throw new TypeCantBeIndexedException(
                lhsType,
                indicesNode.blameToken.get
              )
          }
        }
      requireMatch(requiresReturnType, finalIndexType, indicesNode.blameToken.get)
      false

    case declaration: ASTDeclaration =>
      val nameNode = declaration.jjtGetChild(0).asInstanceOf[ASTName]
      val variableName = nameNode.getName

      // illegal to redeclare a variable. check if it is already in current scope.
      // it *is* legal to redeclare a variable in a different scope where the previous declaration
      // is still visible in outer scope.
      typeContext.lookupTopContext(variableName) match {
        case Some(_) => throw new RedeclarationException(
          variableName,
          nameNode.blameToken.get
        )
        case _ => ()
      }
      val typed = Type.from(
        declaration.jjtGetChild(1).asInstanceOf[ASTType],
        typeContext,
        nameNode.blameToken.get
      )

      // check that the expression matches the expected type
      evaluate(declaration.jjtGetChild(2), typed, typeContext)

      // expression type checks, so add this variable to the type context
      typeContext.put(
        variableName,
        typed
      )
      false

    case send: ASTSend =>
      val anyValueMimicType = AnyValueMimicType()
      evaluate(send.jjtGetChild(0), anyValueMimicType, typeContext)
      send.expectedType = anyValueMimicType

      evaluate(send.jjtGetChild(1), MimicType(), typeContext)

      false

    case blockRecv: ASTBlockRecv =>
      // We know from parsing that the LHS is an lvalue in context
      if (blockRecv.isDecl) {
        val typed = Type.from(blockRecv.jjtGetChild(0).asInstanceOf[ASTType], typeContext, blockRecv.blameToken.get)
        val nameNode = blockRecv.jjtGetChild(1).asInstanceOf[ASTName]
        requireOutOfScope(nameNode.getName, typeContext, nameNode.blameToken.get)

        blockRecv.expectedType = typed

        // check the target is valid
        evaluate(blockRecv.jjtGetChild(2), MimicType(), typeContext)

        // put new decl at the end so it can't be self-referential.
        typeContext.put(nameNode.getName, typed)
      } else {
        val anyValueMimicType = AnyValueMimicType()
        evaluate(blockRecv.jjtGetChild(0), anyValueMimicType, typeContext)
        // likewise, type check the sender to ensure it's a valid target
        evaluate(blockRecv.jjtGetChild(1), MimicType(), typeContext)

        blockRecv.expectedType = anyValueMimicType
      }
      false

    case target: ASTTarget =>
      // This is a hole in the typing system.
      // There is no way to know in advance what other nodes will send us.
      // Therefore, this bypasses any type checks.
      target.target match {
        // must ensure both subnodes (i, j) type check as integers
        case ASTTarget.Target.WORKER =>
          evaluate(target.jjtGetChild(0), IntType(), typeContext)
          evaluate(target.jjtGetChild(1), IntType(), typeContext)
        case _ => ()
      }
      false

    case assignment: ASTAssignment =>
      // As long as everything in an assignment is consistently the same type, it's fine.
      val mimicType = MimicType()

      // We might have a constraint on our type (e.g., if (x = y) { ... } in which case x and y must be INTs)
      // so we need to restrict our type immediately.
      // This match will always succeed, but will restrict our MimicType
      mimicType.matches(requiresReturnType)

      // An assignment only has two sub-nodes
      evaluate(assignment.jjtGetChild(0), mimicType, typeContext)
      evaluate(assignment.jjtGetChild(1), mimicType, typeContext)
      false

    case _: ASTOperator => false // no type checks needed as this is done in the parent nodes

    case logicalAnd: ASTLogicalAnd =>
      requireMatch(requiresReturnType, IntType(), logicalAnd.blameToken.get)
      requireChildrenType(IntType(), logicalAnd, typeContext)

      false

    case logicalOr: ASTLogicalOr =>
      requireMatch(requiresReturnType, IntType(), logicalOr.blameToken.get)
      requireChildrenType(IntType(), logicalOr, typeContext)
      false

    case mathOp: ASTMathOp =>
      requireMatch(requiresReturnType, IntType(), mathOp.jjtGetChild(1).asInstanceOf[ASTOperator].blameToken.get)
      (0 until mathOp.jjtGetNumChildren()) foreach {
        i => evaluate(mathOp.jjtGetChild(i), IntType(), typeContext)
      }
      false

    case eqNeq: ASTEqNeq =>
      requireMatch(
        requiresReturnType,
        IntType(),
        eqNeq.jjtGetChild(1).asInstanceOf[ASTOperator].blameToken.get
      )

      // The first two sides of the comparison must have the same type,
      // and the rest must be integers.
      val mimicType = MimicType()
      (0 until eqNeq.jjtGetNumChildren() by 2) foreach {
        i =>
          evaluate(eqNeq.jjtGetChild(i), if (i/2 <= 1) mimicType else IntType(), typeContext)
      }
      false

    case negation: ASTNegation =>
      requireMatch(requiresReturnType, IntType(), negation.blameToken.get)
      evaluate(negation.jjtGetChild(0), IntType(), typeContext)
      false

    case attributeAccess: ASTAttributeAccess =>
      val mimicType = MimicType()
      evaluate(attributeAccess.jjtGetChild(0), mimicType, typeContext)

      val finalType = ((1 until attributeAccess.jjtGetNumChildren()) foldLeft mimicType.mimickedType.get) {
        (t, i) => t match {
          case StructType(structName, attrs, _) => attributeAccess.jjtGetChild(i) match {
            case indexNode: ASTIndex =>
              val nameNode = indexNode.jjtGetChild(0)
              val attrName = nameNode.asInstanceOf[ASTName].getName
              attrs.get(attrName) match {
                case Some(typed) => // get the type that the attribute will be after indexing
                  val attributeMimicType = MimicType()

                  typeContext.withinFrame {
                    typeContext.put(attrName, typed)
                    evaluate(indexNode, attributeMimicType, typeContext)
                  }

                  attributeMimicType.mimickedType.get
                case None =>
                  throw new InvalidAttributeAccessException(attrName, structName, attributeAccess.blameToken.get);
            }
            case nameNode: ASTName =>
              attrs.get(nameNode.getName) match {
                case Some(v) => v
                case None => throw new InvalidAttributeAccessException(
                  nameNode.getName, structName, attributeAccess.blameToken.get);
              }
//            case _ => assert(false); MimicType()
          }
          case t => throw new NotAStructException(t, attributeAccess.blameToken.get)
        }
      }

      requireMatch(requiresReturnType, finalType, attributeAccess.blameToken.get)
      false

    case number: ASTNumber =>
      requireMatch(requiresReturnType, IntType(), number.blameToken.get)
      false

    case name: ASTName =>
      val typed = typeContext.lookup(name.getName) match {
        case Some(t) => t
        case _ => throw new UndefinedVariableException(
          name.getName,
          name.blameToken.get
        )
      }
      requireMatch(requiresReturnType, typed, name.blameToken.get)
      false

    case unit: ASTUnitValue =>
      requireMatch(requiresReturnType, UnitType(), unit.blameToken.get)
      false

    case optionalValue: ASTOptionalValue =>
      val mimicType = MimicType()
      optionalValue.option match {
        case ASTOptionalValue.Option.NONE => ()
        case ASTOptionalValue.Option.SOME =>
          evaluate(optionalValue.jjtGetChild(0), mimicType, typeContext)
      }
      requireMatch(requiresReturnType, OptionType(mimicType), optionalValue.blameToken.get)
      false

    case arrayValue: ASTArrayValue =>
      // Allow an array of any type
      val mimicType = MimicType()

      // This match checks that we are allowed to form an array
      // If the array has already been typed (e.g., a declaration like var x: array[int] = {1, 2, 3};)
      // then this will morph our MimicType() to the appropriate type
      requireMatch(requiresReturnType, ArrayType(mimicType), arrayValue.blameToken.get)

      // Now, we check that each element of the array match our MimicType() which will update
      // as it matches the elements of the array. We must therefore use the same mimicType in each
      // recursive call.
      val callArguments = arrayValue.jjtGetChild(0)
      0 until callArguments.jjtGetNumChildren() foreach {
        i => evaluate(callArguments.jjtGetChild(i), mimicType, typeContext)
      }
      false

    case arrayAutoCreation: ASTArrayAutoCreation =>
      // array [ TYPE ] ( EXPRESSION , VALUE )
      val contentsType = Type.from(
        arrayAutoCreation.jjtGetChild(0).asInstanceOf[ASTType],
        typeContext,
        arrayAutoCreation.blameToken.get)
      requireMatch(requiresReturnType, ArrayType(contentsType), arrayAutoCreation.blameToken.get)
      evaluate(arrayAutoCreation.jjtGetChild(1), IntType(), typeContext)
      evaluate(arrayAutoCreation.jjtGetChild(2), contentsType, typeContext)
      false

    case myXNode: ASTmyX =>
      requireMatch(requiresReturnType, IntType(), myXNode.blameToken.get)
      false

    case myYNode: ASTmyY =>
      requireMatch(requiresReturnType, IntType(), myYNode.blameToken.get)
      false

    case numRowsNode: ASTnumRows =>
      requireMatch(requiresReturnType, IntType(), numRowsNode.blameToken.get)
      false

    case numColsNode: ASTnumCols =>
      requireMatch(requiresReturnType, IntType(), numColsNode.blameToken.get)
      false
  }

  private def requireChildrenType(typed: Type, node: Node, typeContext: Context[Type]): Unit =
    (0 until node.jjtGetNumChildren()) foreach {
      i => evaluate(node.jjtGetChild(i), typed, typeContext)
    }

  private def requireOutOfScope(name: String, typeContext: Context[Type], blameToken: Token): Unit = {
    typeContext.lookupTopContext(name).map( _ => throw new RedeclarationException(name, blameToken))
  }

  private def requireMatch(required: Type, observed: Type, blameToken: Token): Unit =
    if (!required.matches(observed)) {
      throw new MismatchedTypeException(required, observed, blameToken)
    }

}
