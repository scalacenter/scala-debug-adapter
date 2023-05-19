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
    case _ => Unrecoverable(s"Unsupported literal type: ${value.getClass}")
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
case class PrimitiveBinaryOpTree(
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

case class PrimitiveUnaryOpTree(
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
    case (tree: RuntimeEvaluationTree, Module(module)) => Valid(new OuterModuleTree(ModuleTree(module, None)))
    case (tree: RuntimeEvaluationTree, ct: ClassType) => Valid(new OuterClassTree(tree, ct))
    case _ => Recoverable("No valid outer can be found")
  }
}

case class ThisTree(
    `type`: ReferenceType
) extends RuntimeEvaluationTree {
  override def prettyPrint(depth: Int): String = s"ThisTree(${`type`})"
}

case class ModuleTree(
    `type`: ClassType,
    of: Option[RuntimeEvaluationTree]
) extends RuntimeEvaluationTree
    with TypeTree {
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|ModuleTree(
        |${indent}mod= ${`type`}
        |${indent}of= ${of.map(_.prettyPrint(depth + 1)).getOrElse("None")}
        |${indent.dropRight(1)})""".stripMargin
  }
}

object ModuleTree {
  def apply(`type`: ClassType, of: Option[RuntimeTree]): Validation[ModuleTree] = of match {
    case Some(tree: RuntimeEvaluationTree) => Valid(ModuleTree(`type`, Some(tree)))
    case Some(_: RuntimeValidationTree) => Recoverable("The parent of a module must be evaluable")
    case None => Valid(ModuleTree(`type`, None))
  }

  def apply(`type`: ClassType, of: Option[RuntimeEvaluationTree]): ModuleTree = of match {
    case Some(value) if `type`.name().startsWith(value.`type`.name()) => new ModuleTree(`type`, Some(value))
    case _ => new ModuleTree(`type`, of)
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
