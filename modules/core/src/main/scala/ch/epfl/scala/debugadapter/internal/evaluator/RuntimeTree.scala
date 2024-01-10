package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.*
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.evaluator.RuntimePrimitiveOps.*

/* -------------------------------------------------------------------------- */
/*                              Global hierarchy                              */
/* -------------------------------------------------------------------------- */
sealed trait RuntimeTree {
  def `type`: Type
  override def toString(): String = prettyPrint(0)
  def prettyPrint(depth: Int): String
}

sealed trait RuntimeEvaluableTree extends RuntimeTree

sealed trait ModuleTree extends RuntimeEvaluableTree

sealed trait MethodTree extends RuntimeEvaluableTree {
  def method: Method
  def args: Seq[RuntimeEvaluableTree]
}

sealed trait FieldTree extends AssignableTree {
  def field: Field
  def immutable: Boolean = field.isFinal
}

sealed trait AssignableTree extends RuntimeEvaluableTree

/* -------------------------------------------------------------------------- */
/*                                Simple trees                                */
/* -------------------------------------------------------------------------- */
case class LocalVarTree(
    name: String,
    `type`: Type
) extends AssignableTree {
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
case class BinaryOpTree(
    lhs: RuntimeEvaluableTree,
    rhs: RuntimeEvaluableTree,
    op: BinaryOp
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

object BinaryOpTree {
  def apply(lhs: RuntimeTree, args: Seq[RuntimeEvaluableTree], name: String)(implicit
      logger: Logger
  ): Validation[BinaryOpTree] =
    (lhs, args) match {
      case (ret: RuntimeEvaluableTree, Seq(right)) =>
        BinaryOp(ret, right, name).map(BinaryOpTree(ret, right, _))
      case _ => Recoverable(s"Primitive operation operand must be evaluable")
    }
}

case class ArrayElemTree(array: RuntimeEvaluableTree, index: RuntimeEvaluableTree, `type`: Type)
    extends RuntimeEvaluableTree {
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|ArrayAccessorTree(
        |${indent}array= $array,
        |${indent}index= $index
        |${indent.dropRight(1)})""".stripMargin
  }
}

object ArrayElemTree {
  def apply(tree: RuntimeTree, index: Seq[RuntimeEvaluableTree]): Validation[ArrayElemTree] = {
    val integerTypes = Seq("java.lang.Integer", "java.lang.Short", "java.lang.Byte", "java.lang.Character")
    if (index.size < 1 || index.size > 1) Recoverable("Array accessor must have one argument")
    else
      (tree, tree.`type`) match {
        case (tree: RuntimeEvaluableTree, arr: ArrayType) =>
          index.head.`type` match {
            case idx @ (_: IntegerType | _: ShortType | _: ByteType | _: CharType) =>
              Valid(new ArrayElemTree(tree, index.head, arr.componentType()))
            case ref: ReferenceType if integerTypes.contains(ref.name) =>
              Valid(new ArrayElemTree(tree, index.head, arr.componentType()))
            case _ => Recoverable("Array index must be an integer")
          }
        case _ => Recoverable("Not an array accessor")
      }
  }
}

case class UnaryOpTree(
    rhs: RuntimeEvaluableTree,
    op: UnaryOp
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
object UnaryOpTree {
  def apply(rhs: RuntimeTree, name: String)(implicit logger: Logger): Validation[UnaryOpTree] = rhs match {
    case ret: RuntimeEvaluableTree => UnaryOp(ret, name).map(UnaryOpTree(ret, _))
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
case class NewInstanceTree(init: StaticMethodTree) extends RuntimeEvaluableTree {
  override lazy val `type`: ClassType = init.method.declaringType().asInstanceOf[ClassType]
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|NewInstanceTree(
        |${indent}init= ${init.prettyPrint(depth + 1)}
        |${indent.dropRight(1)})""".stripMargin
  }
}

case class ThisTree(
    `type`: ReferenceType
) extends RuntimeEvaluableTree {
  override def prettyPrint(depth: Int): String = s"ThisTree(${`type`})"
}

object ThisTree {
  def apply(ths: Option[JdiObject])(implicit logger: Logger): Validation[ThisTree] =
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
    init: InstanceMethodTree
) extends ModuleTree {
  override def `type`: ClassType = module
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|NestedModuleTree(
        |${indent}mod= $module
        |${indent}init= ${init.prettyPrint(depth + 1)}
        |${indent.dropRight(1)})""".stripMargin
  }
}

case class ClassTree(
    `type`: ClassType
) extends RuntimeTree {
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
case class RuntimeValueTree(
    value: Safe[JdiValue],
    `type`: Type
) extends RuntimeEvaluableTree {
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|RuntimeValueTree(
        |${indent}v= $value,
        |${indent}t= ${`type`}
        |${indent.dropRight(1)})""".stripMargin
  }
}

/* -------------------------------------------------------------------------- */
/*                             Flow control trees                             */
/* -------------------------------------------------------------------------- */
case class IfTree(
    p: RuntimeEvaluableTree,
    thenp: RuntimeEvaluableTree,
    elsep: RuntimeEvaluableTree,
    `type`: Type
) extends RuntimeEvaluableTree {
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|IfTree(
        |${indent}p= ${p.prettyPrint(depth + 1)},
        |${indent}ifTrue= ${thenp.prettyPrint(depth + 1)},
        |${indent}ifFalse= ${elsep.prettyPrint(depth + 1)}
        |${indent}t= ${`type`}
        |${indent.dropRight(1)})""".stripMargin
  }
}

object IfTree {

  /**
   * Returns the type of the branch that is chosen, if any
   *
   * @param t1
   * @param t2
   * @return Some(true) if t1 is chosen, Some(false) if t2 is chosen, None if no branch is chosen
   */
  def apply(
      p: RuntimeEvaluableTree,
      ifTrue: RuntimeEvaluableTree,
      ifFalse: RuntimeEvaluableTree,
      // ! This is a hack, passing a wrong method would lead to inconsistent trees
      assignableFrom: (Type, Type) => Boolean,
      objType: => Type
  ): Validation[IfTree] = {
    val pType = p.`type`
    val tType = ifTrue.`type`
    val fType = ifFalse.`type`

    if (isBoolean(p.`type`))
      if (assignableFrom(tType, fType)) Valid(IfTree(p, ifTrue, ifFalse, tType))
      else if (assignableFrom(fType, tType)) Valid(IfTree(p, ifTrue, ifFalse, fType))
      else Valid(IfTree(p, ifTrue, ifFalse, objType))
    else CompilerRecoverable("A predicate must be a boolean")
  }

  private def isBoolean(tpe: Type): Boolean =
    tpe.isInstanceOf[BooleanType] || tpe.name == "java.lang.Boolean"
}

/* -------------------------------------------------------------------------- */
/*                                 Assign tree                                */
/* -------------------------------------------------------------------------- */
case class AssignTree(
    lhs: AssignableTree,
    rhs: RuntimeEvaluableTree,
    `type`: Type
) extends RuntimeEvaluableTree {
  override def prettyPrint(depth: Int): String = {
    val indent = "\t" * (depth + 1)
    s"""|AssignTree(
        |${indent}lhs= ${lhs.prettyPrint(depth + 1)},
        |${indent}rhs= ${rhs.prettyPrint(depth + 1)},
        |${indent}t= ${`type`}
        |${indent.dropRight(1)})""".stripMargin
  }
}

object AssignTree {
  def apply(lhs: RuntimeEvaluableTree, rhs: RuntimeEvaluableTree, tpe: Type): Validation[AssignTree] =
    lhs match {
      case lhs: AssignableTree => Valid(AssignTree(lhs, rhs, tpe))
      case _ => CompilerRecoverable("Left hand side of an assignment must be assignable")
    }
}
