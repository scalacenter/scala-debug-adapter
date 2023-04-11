package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._

/* -------------------------------------------------------------------------- */
/*                              Global hierarchy                              */
/* -------------------------------------------------------------------------- */
sealed trait RuntimeValidationTree {
  def `type`: Type
  override def toString() = toString(0)
  def toString(depth: Int): String
  def hasMethodCall: Boolean = this match {
    case mt: ModuleTree => mt.of.map(_.hasMethodCall).getOrElse(false)
    case ft: InstanceFieldTree => ft.qual.hasMethodCall
    case _: InstanceMethodTree | _: StaticMethodTree | _: NewInstanceTree => true
    case _: LiteralTree | _: LocalVarTree | _: ThisTree | _: StaticFieldTree | _: ClassTree | _: PrimitiveMethodTree =>
      false
  }
}

sealed trait RuntimeEvaluationTree extends RuntimeValidationTree

sealed trait TypeTree extends RuntimeValidationTree {
  override def `type`: ClassType
}

sealed trait MethodTree extends RuntimeEvaluationTree {
  def method: Method
}

sealed trait FieldTree extends RuntimeEvaluationTree {
  def field: Field
}

/* -------------------------------------------------------------------------- */
/*                                Simple trees                                */
/* -------------------------------------------------------------------------- */
case class LiteralTree(
    value: JdiValue
) extends RuntimeEvaluationTree {
  override lazy val `type` = value.value.`type`()
  override def toString(depth: Int): String = s"LiteralTree($value)"
}

// TODO: will not work when caching expression since the frame will be different
case class LocalVarTree(
    localVar: LocalVariable
) extends RuntimeEvaluationTree {
  override lazy val `type` = localVar.`type`()
  override def toString(depth: Int): String = s"LocalVarTree($localVar)"
}

/* -------------------------------------------------------------------------- */
/*                                 Field trees                                */
/* -------------------------------------------------------------------------- */
case class InstanceFieldTree(
    field: Field,
    qual: RuntimeEvaluationTree
) extends FieldTree {
  override lazy val `type` = field.`type`()
  override def toString(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|FieldTree(
        |${indent}f= $field,
        |${indent}qual= ${qual.toString(depth + 1)}
        |${indent.dropRight(1)})""".stripMargin
  }
}
case class StaticFieldTree(
    field: Field,
    on: ClassType
) extends FieldTree {
  override lazy val `type` = field.`type`()
  override def toString(depth: Int): String = {
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
case class PrimitiveMethodTree(
    lhs: RuntimeValidationTree,
    rhs: RuntimeValidationTree,
    op: RuntimePrimitiveOp
) extends RuntimeEvaluationTree {
  override lazy val `type`: PrimitiveType = op.typeWith(lhs.`type`, rhs.`type`)
  override def toString(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|PrimitiveMethodTree(
        |${indent}lhs= ${lhs.toString(depth + 1)},
        |${indent}rhs= ${rhs.toString(depth + 1)},
        |${indent}op= $op
        |${indent.dropRight(1)})""".stripMargin
  }
}

case class InstanceMethodTree(
    method: Method,
    args: Seq[RuntimeValidationTree],
    qual: RuntimeEvaluationTree
) extends MethodTree {
  override lazy val `type` = method.returnType()
  override def toString(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|MethodTree(
        |${indent}m= $method,
        |${indent}args= ${args.map(_.toString(depth + 1)).mkString(",\n" + indent)},
        |${indent}qual= ${qual.toString(depth + 1)}
        |${indent.dropRight(1)})""".stripMargin
  }
}
case class StaticMethodTree(
    method: Method,
    args: Seq[RuntimeValidationTree],
    on: ClassType
) extends MethodTree {
  override lazy val `type` = method.returnType()
  override def toString(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|StaticMethodTree(
        |${indent}m= $method,
        |${indent}args= ${args.map(_.toString(depth + 1)).mkString(",\n" + indent)},
        |${indent}on= $on
        |${indent.dropRight(1)})""".stripMargin
  }
}

/* -------------------------------------------------------------------------- */
/*                                 Class trees                                */
/* -------------------------------------------------------------------------- */
case class NewInstanceTree(method: Method, args: Seq[RuntimeValidationTree]) extends RuntimeEvaluationTree {
  override lazy val `type`: ClassType = method.declaringType().asInstanceOf[ClassType]
  override def toString(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|NewInstanceTree(
        |${indent}m= $method,
        |${indent}args= ${args.map(_.toString(depth + 1)).mkString(",\n" + indent)}
        |${indent.dropRight(1)})""".stripMargin
  }
}

case class ThisTree(
    value: JdiObject
) extends RuntimeEvaluationTree {
  override lazy val `type`: ReferenceType = value.reference.referenceType()
  override def toString(depth: Int): String = s"ThisTree($value)"
}

case class ModuleTree(
    `type`: ClassType,
    of: Option[RuntimeValidationTree]
) extends RuntimeEvaluationTree
    with TypeTree {
  def toString(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|ModuleTree(
        |${indent}mod= ${`type`}
        |${indent}of= $of
        |${indent.dropRight(1)})""".stripMargin
  }
}

case class ClassTree(
    `type`: ClassType
) extends TypeTree {
  override def toString(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|ModuleTree(
        |${indent}t= ${`type`},
        |${indent.dropRight(1)})""".stripMargin
  }
}
