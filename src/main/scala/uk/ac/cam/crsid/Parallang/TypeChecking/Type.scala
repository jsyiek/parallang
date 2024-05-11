package uk.ac.cam.crsid.Parallang.TypeChecking

import uk.ac.cam.crsid.Parallang.Parser.ASTType.Type._
import uk.ac.cam.crsid.Parallang.Parser.{ASTDefnArguments, ASTName, ASTType, Token}
import uk.ac.cam.crsid.Parallang.TypeChecking.Exception.UndefinedStructException
import uk.ac.cam.crsid.lib.Collection.Context

import scala.collection.mutable

object Type {
  def parseStruct(name: String, typeContext: Context[Type], blameToken: Token): StructType =
    typeContext.lookup(name) match {
      case Some(StructType(name, attrs, _)) => StructType(name, attrs, false)
      case _ => throw new UndefinedStructException(name, blameToken)
    }

  def from(node: ASTType,
           typeContext: Context[Type],
           blameToken: Token): Type = node.getType match {
    case INT        => IntType()
    case UNIT       => UnitType()
    case STRUCT     =>
      val name = node.jjtGetChild(0).asInstanceOf[ASTName].getName
      parseStruct(name, typeContext, blameToken)
    case OPTION     => OptionType(from(node.jjtGetChild(0).asInstanceOf[ASTType], typeContext, blameToken))
    case ARRAY      => ArrayType(from(node.jjtGetChild(0).asInstanceOf[ASTType], typeContext, blameToken))
  }

  def defnArgumentsToList(defnArguments: ASTDefnArguments, typeContext: Context[Type], blameToken: Token): IndexedSeq[Type] = {
    1 until defnArguments.jjtGetNumChildren() by 2 map {
      (i: Int) => Type.from(defnArguments.jjtGetChild(i).asInstanceOf[ASTType], typeContext, blameToken)
    }
  }

  def defnArgumentsToLinkedHashMap(defnArguments: ASTDefnArguments,
                                   typeContext: Context[Type],
                                   blameToken: Token): mutable.LinkedHashMap[String, Type] = {
    val map = mutable.LinkedHashMap[String, Type]()
    ((0 until defnArguments.jjtGetNumChildren() by 2) zip (1 until defnArguments.jjtGetNumChildren() by 2))
      .foreach {
        case (nameIndex, typeIndex) =>
          val name = defnArguments.jjtGetChild(nameIndex).asInstanceOf[ASTName].getName
          val typed = Type.from(
            defnArguments.jjtGetChild(typeIndex).asInstanceOf[ASTType],
            typeContext,
            blameToken
          )
          map(name) = typed
      }
    map
  }
}

abstract class Type {
  def matches(other: Type): Boolean = other match {
    case NoReturnAnyType() => true
    case MimicType() => other.matches(this)
    case _ => false
  }
  // Useful for function-like constructs
//  def accepts(args: IndexedSeq[Type]): Boolean = false
}

case class ArrayType(of: Type) extends Type {
  override def matches(other: Type): Boolean = other match {
    case ArrayType(ofOther) => of.matches(ofOther)
    case _ => super.matches(other)
  }

  override def toString: String = s"array[$of]"
}

case class OptionType(of: Type) extends Type {
  override def matches(other: Type): Boolean = other match {
    case OptionType(ofOther) => of.matches(ofOther)
    case _ => super.matches(other)
  }

  override def toString: String = s"option[$of]"
}

case class UnitType() extends Type {
  override def matches(other: Type): Boolean = other match {
    case UnitType() => true
    case _ => super.matches(other)
  }

  override def toString: String = "unit"
}

// We need to use the linked list hash map to ensure values stay in the correct order.
case class StructType(name: String, attrs: mutable.LinkedHashMap[String, Type], isInitializer: Boolean=false) extends Type {
  override def matches(other: Type): Boolean = other match {
    case StructType(otherName, otherAttrs, otherIsInitializer) =>
      isInitializer == otherIsInitializer && otherName == name && otherAttrs == attrs
    case _ => super.matches(other)
  }

  override def toString: String = {
    val params: List[String] = (attrs foldLeft List[String]()) {
      (l: List[String], p) => p match {
        case (k: String, v: Type) => s"$k: $v" :: l
      }
    }
    val typeType =
      if (isInitializer)
        "StructType"
      else
        "struct"
    s"$typeType $name (${params.mkString(", ")})"
  }
}

case class IntType() extends Type {
  override def matches(other: Type): Boolean = other match {
    case IntType() | NoReturnAnyType() => true
    case _ => super.matches(other)
  }

  override def toString: String = "int"
}

case class FunctionType(argsType: IndexedSeq[Type], returnType: Type) extends Type {
  /* Not implemented for functions */
  override def matches(other: Type): Boolean = false
}

// Return statement must detect and reject this type
// It prevents top-level return statements
case class NoReturnAnyType() extends Type {
  override def matches(other: Type): Boolean = true

  override def toString: String = "<Empty Type>"
}

// Refuses to match FunctionTypes or StructType initializers
case class AnyValueType() extends Type {
  override def matches(other: Type): Boolean = other match {
    case FunctionType(_, _) | StructType(_, _, true) => false
    case _ => true
  }

  override def toString: String = "<Any Value Type>"
}

// It morphs into the first type it is compared against.
case class MimicType() extends Type {
  var mimickedType: Option[Type] = None
  override def matches(other: Type): Boolean = mimickedType match {
    case None => other match {
      // A slight subtlety with an NoReturnAnyType() is that allowing MimicType
      // to move into it will allow expressions to bypass type checking
      // in arrays
      // E.g., the SoloExpression {1, {2, 3}} should fail type-checking, but unifying
      // Array[MimicType()] with the top-level Array[NoReturnAnyType()] will cause the former type to morph
      // to Array[NoReturnAnyType()] and type check the array.
      case NoReturnAnyType() => true
      case other => mimickedType = Some(other); true
    }
    case Some(morph) => morph.matches(other)
  }

  override def toString: String = mimickedType match {
    case None => "<...>"
    case Some(t) => t.toString
  }
}

// Matches only value types (so no FunctionTypes or StructType initializers)
// However, it also remembers what types it has matched against
case class AnyValueMimicType() extends Type {
  var mimickedType: Option[Type] = None
  override def matches(other: Type): Boolean = mimickedType match {
    case None => other match {
      case NoReturnAnyType() => true
      case FunctionType(_, _) | StructType(_, _, true) => false
      case other => mimickedType = Some(other); true
    }
    case Some(morph) => other match {
      case AnyValueMimicType() => other.matches(morph)
      case _ => morph.matches(other)
    }
  }

  override def toString: String = mimickedType match {
    case None => "<...>"
    case Some(s) => "AnyValueMimicType: "+s.toString
  }
}