package ch.epfl.scala.debugadapter.internal.evaluator

import scala.meta.Term
import scala.meta.Defn
import scala.meta.Mod
import com.sun.jdi.{ClassType, Field, ReferenceType, Type}
import scala.jdk.CollectionConverters.*

private[internal] object RuntimeEvaluatorBooleanPatterns {
  object ColonEndingInfix {
    @inline def unapply(stat: Term.ApplyInfix): Option[Term.ApplyInfix] =
      stat match {
        case Term.ApplyInfix.After_4_6_0(_, op, _, _) if (op.value.endsWith(":")) => Some(stat)
        case _ => None
      }
  }

  object LazyDefine {
    @inline def unapply(stat: Defn.Val): Option[Defn.Val] =
      stat match {
        case Defn.Val(mods, _, _, _) if (mods.contains(Mod.Lazy)) => None
        case _ => Some(stat)
      }
  }

  object Module {
    @inline def isModule(tpe: Type): Boolean = {
      tpe match {
        case ref: ReferenceType =>
          ref.fields().asScala.find(_.name() == "MODULE$").isDefined
        case _ => false
      }
    }

    @inline def unapply(field: Field): Option[ClassType] = {
      field.`type`() match {
        case ref: ClassType if isModule(ref) => Some(ref)
        case _ => None
      }
    }

    @inline def unapply(tree: RuntimeValidationTree): Option[ClassType] =
      tree match {
        case mt: ModuleTree => Some(mt.`type`)
        case ft: InstanceFieldTree => unapply(ft.field)
        case sft: StaticFieldTree => unapply(sft.field)
        case tt: ThisTree =>
          if (isModule(tt.`type`)) Some(tt.`type`.asInstanceOf[ClassType])
          else None
        case mt: InstanceMethodTree =>
          if (isModule(mt.`type`)) Some(mt.`type`.asInstanceOf[ClassType])
          else None
        case smt: StaticMethodTree =>
          if (isModule(smt.`type`)) Some(smt.`type`.asInstanceOf[ClassType])
          else None
        case _: LiteralTree | _: LocalVarTree | _: NewInstanceTree | _: ClassTree | _: PrimitiveMethodTree => None
      }
  }

  object MethodCall {
    @inline def unapply(tree: Valid[RuntimeValidationTree]): Option[RuntimeValidationTree] =
      tree.toOption.filter(_.hasMethodCall)
  }

  object ReferenceTree {
    @inline def unapply(tree: Validation[RuntimeValidationTree]): Option[ReferenceType] = {
      if (tree.isInvalid) None
      else
        tree.get.`type` match {
          case ref: ReferenceType => Some(ref)
          case _ => None
        }
    }
  }
}
