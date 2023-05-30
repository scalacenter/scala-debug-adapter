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
sealed trait RuntimeEvaluationTree extends RuntimeTree

sealed trait TypeTree extends RuntimeTree {
  override def `type`: ClassType
}

sealed trait ModuleTree extends RuntimeEvaluationTree with TypeTree

sealed trait MethodTree extends RuntimeEvaluationTree {
  def method: Method
}

sealed trait FieldTree extends RuntimeEvaluationTree {
  def field: Field
}

sealed trait OuterTree extends RuntimeEvaluationTree

/* -------------------------------------------------------------------------- */
/*                                Simple trees                                */
/* -------------------------------------------------------------------------- */
case class LiteralTree private (
    value: Safe[Any],
    `type`: Type
) extends RuntimeEvaluationTree {
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
) extends RuntimeEvaluationTree {
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
    qual: RuntimeEvaluationTree
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
    lhs: RuntimeEvaluationTree,
    rhs: RuntimeEvaluationTree,
    op: RuntimeBinaryOp
) extends RuntimeEvaluationTree {
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
  def apply(lhs: RuntimeTree, args: Seq[RuntimeEvaluationTree], name: String): Validation[PrimitiveBinaryOpTree] =
    (lhs, args) match {
      case (ret: RuntimeEvaluationTree, Seq(right)) =>
        RuntimeBinaryOp(ret, right, name).map(PrimitiveBinaryOpTree(ret, right, _))
      case _ => Recoverable(s"Primitive operation operand must be evaluable")
    }
}

case class PrimitiveUnaryOpTree private (
    rhs: RuntimeEvaluationTree,
    op: RuntimeUnaryOp
) extends RuntimeEvaluationTree {
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
    case ret: RuntimeEvaluationTree => RuntimeUnaryOp(ret, name).map(PrimitiveUnaryOpTree(ret, _))
    case _ => Recoverable(s"Primitive operation operand must be evaluable")
  }
}

case class InstanceMethodTree(
    method: Method,
    args: Seq[RuntimeEvaluationTree],
    qual: RuntimeEvaluationTree
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
    args: Seq[RuntimeEvaluationTree],
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
case class NewInstanceTree(method: Method, args: Seq[RuntimeEvaluationTree]) extends RuntimeEvaluationTree {
  override lazy val `type`: ClassType = method.declaringType().asInstanceOf[ClassType]
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|NewInstanceTree(
        |${indent}m= $method,
        |${indent}args= ${args.map(_.prettyPrint(depth + 1)).mkString(",\n" + indent)}
        |${indent.dropRight(1)})""".stripMargin
  }
}

case class OuterClassTree(
    inner: RuntimeEvaluationTree,
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
    case (tree: RuntimeEvaluationTree, Module(module)) => Valid(new OuterModuleTree(TopLevelModuleTree(module)))
    case (tree: RuntimeEvaluationTree, ct: ClassType) => Valid(new OuterClassTree(tree, ct))
    case _ => Recoverable("No valid outer can be found")
  }
}

case class ThisTree(
    `type`: ReferenceType
) extends RuntimeEvaluationTree {
  override def prettyPrint(depth: Int): String = s"ThisTree(${`type`})"
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
    of: RuntimeEvaluationTree
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
