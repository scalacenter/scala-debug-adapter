package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._
import RuntimeEvaluatorExtractors.{IsAnyVal, Module}
import scala.util.Success

/* -------------------------------------------------------------------------- */
/*                              Global hierarchy                              */
/* -------------------------------------------------------------------------- */
sealed trait RuntimeTree {
  def `type`: Type
  override def toString(): String = prettyPrint(0)
  def prettyPrint(depth: Int): String
}
sealed trait RuntimeValidationTree extends RuntimeTree
sealed trait RuntimeEvaluableTree extends RuntimeTree

sealed trait TypeTree extends RuntimeTree {
  override def `type`: ClassType
}

sealed trait ModuleTree extends RuntimeEvaluableTree with TypeTree

sealed trait MethodTree extends RuntimeEvaluableTree {
  def method: Method
}

sealed trait FieldTree extends RuntimeEvaluableTree {
  def field: Field
}

sealed trait OuterTree extends RuntimeEvaluableTree

/* -------------------------------------------------------------------------- */
/*                                Simple trees                                */
/* -------------------------------------------------------------------------- */
case class LiteralTree private (
    value: Safe[Any],
    `type`: Type
) extends RuntimeEvaluableTree {
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|LiteralTree(
        |${indent}value= $value,
        |${indent}type= ${`type`}
        |${indent.dropRight(1)})""".stripMargin
  }
}

object LiteralTree {
  def apply(value: (Safe[Any], Type)): Validation[LiteralTree] = value._1 match {
    case Safe(Success(_: String)) | Safe(Success(IsAnyVal(_))) => Valid(new LiteralTree(value._1, value._2))
    case _ => Fatal(s"Unsupported literal type: ${value.getClass}")
  }
}

case class LocalVarTree(
    name: String,
    `type`: Type
) extends RuntimeEvaluableTree {
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|LocalVarTree(
        |${indent}name= $name,
        |${indent}type= ${`type`}
        |${indent.dropRight(1)})""".stripMargin
  }
}

/* -------------------------------------------------------------------------- */
/*                                 Field trees                                */
/* -------------------------------------------------------------------------- */
case class InstanceFieldTree(
    field: Field,
    qual: RuntimeEvaluableTree
) extends FieldTree {
  override lazy val `type` = field.`type`()
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|FieldTree(
        |${indent}f= $field,
        |${indent}qual= ${qual.prettyPrint(depth + 1)}
        |${indent.dropRight(1)})""".stripMargin
  }
}
case class StaticFieldTree(
    field: Field,
    on: ClassType
) extends FieldTree {
  override lazy val `type` = field.`type`()
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|StaticFieldTree(
        |${indent}f= $field,
        |${indent}on= $on
        |${indent.dropRight(1)})""".stripMargin
  }
}

/* -------------------------------------------------------------------------- */
/*                                Method trees                                */
/* -------------------------------------------------------------------------- */
case class PrimitiveBinaryOpTree private (
    lhs: RuntimeEvaluableTree,
    rhs: RuntimeEvaluableTree,
    op: RuntimeBinaryOp
) extends RuntimeEvaluableTree {
  override lazy val `type` = op.typeCheck(lhs.`type`, rhs.`type`)
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|PrimitiveMethodTree(
        |${indent}lhs= ${lhs.prettyPrint(depth + 1)},
        |${indent}rhs= ${rhs.prettyPrint(depth + 1)},
        |${indent}op= $op
        |${indent.dropRight(1)})""".stripMargin
  }
}

object PrimitiveBinaryOpTree {
  def apply(lhs: RuntimeTree, args: Seq[RuntimeEvaluableTree], name: String): Validation[PrimitiveBinaryOpTree] =
    (lhs, args) match {
      case (ret: RuntimeEvaluableTree, Seq(right)) =>
        RuntimeBinaryOp(ret, right, name).map(PrimitiveBinaryOpTree(ret, right, _))
      case _ => Recoverable(s"Primitive operation operand must be evaluable")
    }
}

case class ArrayAccessorTree private (array: RuntimeEvaluableTree, index: RuntimeEvaluableTree, `type`: Type)
    extends RuntimeEvaluableTree {
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|ArrayAccessorTree(
        |${indent}array= $array,
        |${indent}index= $index
        |${indent.dropRight(1)})""".stripMargin
  }
}

object ArrayAccessorTree {
  def apply(tree: RuntimeTree, funName: String, index: Seq[RuntimeEvaluableTree]): Validation[ArrayAccessorTree] = {
    val integerTypes = Seq("java.lang.Integer", "java.lang.Short", "java.lang.Byte", "java.lang.Character")
    if (funName != "apply") Recoverable("Not an array accessor")
    else if (index.size < 1 || index.size > 1) Recoverable("Array accessor must have one argument")
    else
      (tree, tree.`type`) match {
        case (tree: RuntimeEvaluableTree, arr: ArrayType) =>
          index.head.`type` match {
            case idx @ (_: IntegerType | _: ShortType | _: ByteType | _: CharType) =>
              Valid(new ArrayAccessorTree(tree, index.head, arr.componentType()))
            case ref: ReferenceType if integerTypes.contains(ref.name) =>
              Valid(new ArrayAccessorTree(tree, index.head, arr.componentType()))
            case _ => Recoverable("Array index must be an integer")
          }
        case _ => Recoverable("Not an array accessor")
      }
  }
}

case class PrimitiveUnaryOpTree private (
    rhs: RuntimeEvaluableTree,
    op: RuntimeUnaryOp
) extends RuntimeEvaluableTree {
  override lazy val `type` = op.typeCheck(rhs.`type`)
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|PrimitiveMethodTree(
        |${indent}rhs= ${rhs.prettyPrint(depth + 1)},
        |${indent}op= $op
        |${indent.dropRight(1)})""".stripMargin
  }
}
object PrimitiveUnaryOpTree {
  def apply(rhs: RuntimeTree, name: String): Validation[PrimitiveUnaryOpTree] = rhs match {
    case ret: RuntimeEvaluableTree => RuntimeUnaryOp(ret, name).map(PrimitiveUnaryOpTree(ret, _))
    case _ => Recoverable(s"Primitive operation operand must be evaluable")
  }
}

case class InstanceMethodTree(
    method: Method,
    args: Seq[RuntimeEvaluableTree],
    qual: RuntimeEvaluableTree
) extends MethodTree {
  override lazy val `type` = method.returnType()
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|InstanceMethodTree(
        |${indent}m= $method -> ${method.returnType()},
        |${indent}args= ${args.map(_.prettyPrint(depth + 1)).mkString(",\n" + indent)},
        |${indent}qual= ${qual.prettyPrint(depth + 1)}
        |${indent.dropRight(1)})""".stripMargin
  }
}
case class StaticMethodTree(
    method: Method,
    args: Seq[RuntimeEvaluableTree],
    on: ClassType
) extends MethodTree {
  override lazy val `type` = method.returnType()
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|StaticMethodTree(
        |${indent}m= $method,
        |${indent}args= ${args.map(_.prettyPrint(depth + 1)).mkString(",\n" + indent)},
        |${indent}on= $on
        |${indent.dropRight(1)})""".stripMargin
  }
}

/* -------------------------------------------------------------------------- */
/*                                 Class trees                                */
/* -------------------------------------------------------------------------- */
case class NewInstanceTree(mt: MethodTree, args: Seq[RuntimeEvaluableTree]) extends RuntimeEvaluableTree {
  override lazy val `type`: ClassType = mt.method.declaringType().asInstanceOf[ClassType]
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|NewInstanceTree(
        |${indent}m= $mt,
        |${indent}args= ${args.map(_.prettyPrint(depth + 1)).mkString(",\n" + indent)}
        |${indent.dropRight(1)})""".stripMargin
  }
}

case class OuterClassTree(
    inner: RuntimeEvaluableTree,
    `type`: ClassType
) extends OuterTree {
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|OuterClassTree(
        |${indent}of= ${inner.prettyPrint(depth + 1)}
        |${indent}type= ${`type`}
        |${indent.dropRight(1)})""".stripMargin
  }
}

case class OuterModuleTree(
    module: ModuleTree
) extends OuterTree {
  override def `type`: ClassType = module.`type`
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|OuterModuleTree(
        |${indent}module= ${module.prettyPrint(depth + 1)}
        |${indent.dropRight(1)})""".stripMargin
  }
}

object OuterTree {
  def apply(of: RuntimeTree, tpe: Type): Validation[OuterTree] = (of, tpe) match {
    case (tree: RuntimeEvaluableTree, Module(module)) => Valid(new OuterModuleTree(TopLevelModuleTree(module)))
    case (tree: RuntimeEvaluableTree, ct: ClassType) => Valid(new OuterClassTree(tree, ct))
    case _ => Recoverable("No valid outer can be found")
  }
}

case class ThisTree(
    `type`: ReferenceType
) extends RuntimeEvaluableTree {
  override def prettyPrint(depth: Int): String = s"ThisTree(${`type`})"
}

object ThisTree {
  def apply(ths: Option[JdiObject]): Validation[ThisTree] =
    Validation.fromOption(ths).map(ths => ThisTree(ths.reference.referenceType()))
}

case class TopLevelModuleTree(
    `type`: ClassType
) extends ModuleTree {
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|TopLevelModuleTree(
        |${indent}mod= ${`type`}
        |${indent.dropRight(1)})""".stripMargin
  }
}
case class NestedModuleTree(
    module: ClassType,
    of: RuntimeEvaluableTree
) extends ModuleTree {
  override def `type`: ClassType = module
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|NestedModuleTree(
        |${indent}mod= ${module}
        |${indent}of= ${of.prettyPrint(depth + 1)}
        |${indent.dropRight(1)})""".stripMargin
  }
}

case class ClassTree(
    `type`: ClassType
) extends RuntimeValidationTree
    with TypeTree {
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|ClassTree(
        |${indent}t= ${`type`},
        |${indent.dropRight(1)})""".stripMargin
  }
}

/* -------------------------------------------------------------------------- */
/*                             Pre-evaluated trees                            */
/* -------------------------------------------------------------------------- */
case class PreEvaluatedTree(
    value: Safe[JdiValue],
    `type`: Type
) extends RuntimeEvaluableTree {
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|PreEvaluatedTree(
        |${indent}v= $value,
        |${indent}t= ${`type`}
        |${indent.dropRight(1)})""".stripMargin
  }
}

object PreEvaluatedTree {
  def apply(value: (Safe[JdiValue], Type)) = new PreEvaluatedTree(value._1, value._2)
}
