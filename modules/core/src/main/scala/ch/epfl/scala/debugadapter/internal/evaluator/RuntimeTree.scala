package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.evaluator.RuntimePrimitiveOps.*

sealed trait RuntimeEvaluationTree {
  def `type`: jdi.Type
  override def toString(): String = prettyPrint(0)
  def prettyPrint(depth: Int): String
}

object RuntimeEvaluationTree {
  sealed trait CallMethod extends RuntimeEvaluationTree {
    def method: jdi.Method
    def args: Seq[RuntimeEvaluationTree]
  }

  sealed trait Assignable extends RuntimeEvaluationTree

  sealed trait Field extends Assignable {
    def field: jdi.Field
    def immutable: Boolean = field.isFinal
  }

  case class LocalVar(name: String, `type`: jdi.Type) extends Assignable {
    override def prettyPrint(depth: Int): String = {
      val indent = "\t" * (depth + 1)
      s"""|LocalVar(
          |${indent}name= $name,
          |${indent}type= ${`type`}
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class InstanceField(field: jdi.Field, qualifier: RuntimeEvaluationTree) extends Field {
    override lazy val `type` = field.`type`()
    override def prettyPrint(depth: Int): String = {
      val indent = "\t" * (depth + 1)
      s"""|InstanceField(
          |${indent}field = $field,
          |${indent}qualifier = ${qualifier.prettyPrint(depth + 1)}
          |${indent.dropRight(1)})""".stripMargin
    }
  }
  case class StaticField(field: jdi.Field) extends Field {
    override lazy val `type` = field.`type`()
    override def prettyPrint(depth: Int): String = {
      val indent = "\t" * (depth + 1)
      s"""|StaticField(
          |${indent}field = $field
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class CallInstanceMethod(
      method: jdi.Method,
      args: Seq[RuntimeEvaluationTree],
      qualifier: RuntimeEvaluationTree
  ) extends CallMethod {
    override lazy val `type` = method.returnType()
    override def prettyPrint(depth: Int): String = {
      val indent = "\t" * (depth + 1)
      s"""|InstanceMethod(
          |${indent}method = $method -> ${method.returnType()},
          |${indent}args = ${args.map(_.prettyPrint(depth + 1)).mkString(",\n" + indent)},
          |${indent}qualifier = ${qualifier.prettyPrint(depth + 1)}
          |${indent.dropRight(1)})""".stripMargin
    }
  }
  case class CallStaticMethod(
      method: jdi.Method,
      args: Seq[RuntimeEvaluationTree],
      qualifier: jdi.ReferenceType
  ) extends CallMethod {
    override lazy val `type` = method.returnType()
    override def prettyPrint(depth: Int): String = {
      val indent = "\t" * (depth + 1)
      s"""|CallStaticMethod(
          |${indent}m= $method,
          |${indent}args= ${args.map(_.prettyPrint(depth + 1)).mkString(",\n" + indent)},
          |${indent}qualifier= $qualifier
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class CallBinaryOp(
      lhs: RuntimeEvaluationTree,
      rhs: RuntimeEvaluationTree,
      op: BinaryOp
  ) extends RuntimeEvaluationTree {
    override lazy val `type` = op.typeCheck(lhs.`type`, rhs.`type`)
    override def prettyPrint(depth: Int): String = {
      val indent = "\t" * (depth + 1)
      s"""|CallBinaryOp(
          |${indent}lhs = ${lhs.prettyPrint(depth + 1)},
          |${indent}rhs = ${rhs.prettyPrint(depth + 1)},
          |${indent}op = $op
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  object CallBinaryOp {
    def apply(lhs: RuntimeEvaluationTree, args: Seq[RuntimeEvaluationTree], name: String)(implicit
        logger: Logger
    ): Validation[CallBinaryOp] =
      args match {
        case Seq(rhs) => BinaryOp(lhs, rhs, name).map(CallBinaryOp(lhs, rhs, _))
        case _ => Recoverable(s"Too many arguments")
      }
  }

  case class ArrayElem(array: RuntimeEvaluationTree, index: RuntimeEvaluationTree, `type`: jdi.Type)
      extends RuntimeEvaluationTree {
    override def prettyPrint(depth: Int): String = {
      val indent = "\t" * (depth + 1)
      s"""|ArrayElem(
          |${indent}array = $array,
          |${indent}index = $index
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  object ArrayElem {
    def apply(tree: RuntimeEvaluationTree, index: Seq[RuntimeEvaluationTree]): Validation[ArrayElem] = {
      val integerTypes = Seq("java.lang.Integer", "java.lang.Short", "java.lang.Byte", "java.lang.Character")
      if (index.size < 1 || index.size > 1) Recoverable("Array accessor must have one argument")
      else
        (tree, tree.`type`) match {
          case (tree: RuntimeEvaluationTree, arr: jdi.ArrayType) =>
            index.head.`type` match {
              case idx @ (_: jdi.IntegerType | _: jdi.ShortType | _: jdi.ByteType | _: jdi.CharType) =>
                Valid(new ArrayElem(tree, index.head, arr.componentType()))
              case ref: jdi.ReferenceType if integerTypes.contains(ref.name) =>
                Valid(new ArrayElem(tree, index.head, arr.componentType()))
              case _ => Recoverable("Array index must be an integer")
            }
          case _ => Recoverable("Not an array accessor")
        }
    }
  }

  case class CallUnaryOp(
      rhs: RuntimeEvaluationTree,
      op: UnaryOp
  ) extends RuntimeEvaluationTree {
    override lazy val `type` = op.typeCheck(rhs.`type`)
    override def prettyPrint(depth: Int): String = {
      val indent = "\t" * (depth + 1)
      s"""|UnaryOpCall(
          |${indent}rhs= ${rhs.prettyPrint(depth + 1)},
          |${indent}op= $op
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  object CallUnaryOp {
    def apply(rhs: RuntimeEvaluationTree, name: String)(implicit logger: Logger): Validation[CallUnaryOp] = {
      UnaryOp(rhs, name).map(CallUnaryOp(rhs, _))
    }
  }

  case class NewInstance(init: CallStaticMethod) extends RuntimeEvaluationTree {
    override lazy val `type`: jdi.ReferenceType = init.method.declaringType() // .asInstanceOf[jdi.ClassType]
    override def prettyPrint(depth: Int): String = {
      val indent = "\t" * (depth + 1)
      s"""|NewInstance(
          |${indent}init= ${init.prettyPrint(depth + 1)}
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class This(`type`: jdi.ReferenceType) extends RuntimeEvaluationTree {
    override def prettyPrint(depth: Int): String = s"This(${`type`})"
  }

  object This {
    def apply(ths: Option[JdiObject])(implicit logger: Logger): Validation[This] =
      Validation.fromOption(ths).map(ths => This(ths.reference.referenceType()))
  }

  case class StaticModule(
      `type`: jdi.ReferenceType
  ) extends RuntimeEvaluationTree {
    override def prettyPrint(depth: Int): String = {
      val indent = "\t" * (depth + 1)
      s"""|StaticModule(
          |${indent}mod= ${`type`}
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class NestedModule(`type`: jdi.ReferenceType, init: CallInstanceMethod) extends RuntimeEvaluationTree {
    override def prettyPrint(depth: Int): String = {
      val indent = "\t" * (depth + 1)
      s"""|NestedModule(
          |${indent}type = ${`type`}
          |${indent}init = ${init.prettyPrint(depth + 1)}
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class Value(
      value: Safe[JdiValue],
      `type`: jdi.Type
  ) extends RuntimeEvaluationTree {
    override def prettyPrint(depth: Int): String = {
      val indent = "\t" * (depth + 1)
      s"""|Value(
          |${indent}v= $value,
          |${indent}t= ${`type`}
          |${indent.dropRight(1)})""".stripMargin
    }
  }
  case class If(
      p: RuntimeEvaluationTree,
      thenp: RuntimeEvaluationTree,
      elsep: RuntimeEvaluationTree,
      `type`: jdi.Type
  ) extends RuntimeEvaluationTree {
    override def prettyPrint(depth: Int): String = {
      val indent = "\t" * (depth + 1)
      s"""|If(
          |${indent}p= ${p.prettyPrint(depth + 1)},
          |${indent}ifTrue= ${thenp.prettyPrint(depth + 1)},
          |${indent}ifFalse= ${elsep.prettyPrint(depth + 1)}
          |${indent}t= ${`type`}
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  object If {

    /**
     * Returns the type of the branch that is chosen, if any
     *
     * @param t1
     * @param t2
     * @return Some(true) if t1 is chosen, Some(false) if t2 is chosen, None if no branch is chosen
     */
    def apply(
        p: RuntimeEvaluationTree,
        ifTrue: RuntimeEvaluationTree,
        ifFalse: RuntimeEvaluationTree,
        // ! This is a hack, passing a wrong method would lead to inconsistent trees
        assignableFrom: (jdi.Type, jdi.Type) => Boolean,
        objType: => jdi.Type
    ): Validation[If] = {
      val pType = p.`type`
      val tType = ifTrue.`type`
      val fType = ifFalse.`type`

      if (isBoolean(p.`type`))
        if (assignableFrom(tType, fType)) Valid(If(p, ifTrue, ifFalse, tType))
        else if (assignableFrom(fType, tType)) Valid(If(p, ifTrue, ifFalse, fType))
        else Valid(If(p, ifTrue, ifFalse, objType))
      else CompilerRecoverable("A predicate must be a boolean")
    }

    private def isBoolean(tpe: jdi.Type): Boolean =
      tpe.isInstanceOf[jdi.BooleanType] || tpe.name == "java.lang.Boolean"
  }

  case class Assign(
      lhs: Assignable,
      rhs: RuntimeEvaluationTree,
      `type`: jdi.Type
  ) extends RuntimeEvaluationTree {
    override def prettyPrint(depth: Int): String = {
      val indent = "\t" * (depth + 1)
      s"""|Assign(
          |${indent}lhs= ${lhs.prettyPrint(depth + 1)},
          |${indent}rhs= ${rhs.prettyPrint(depth + 1)},
          |${indent}t= ${`type`}
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  object Assign {
    def apply(lhs: RuntimeEvaluationTree, rhs: RuntimeEvaluationTree, tpe: jdi.Type): Validation[Assign] =
      lhs match {
        case lhs: Assignable => Valid(Assign(lhs, rhs, tpe))
        case _ => CompilerRecoverable("Left hand side of an assignment must be assignable")
      }
  }
}
