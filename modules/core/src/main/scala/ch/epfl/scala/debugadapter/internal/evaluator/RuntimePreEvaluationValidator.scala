package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import scala.util.Success
import scala.meta.Term
import scala.meta.Lit

import ch.epfl.scala.debugadapter.internal.SourceLookUpProvider

class RuntimePreEvaluationValidator(
    override val frame: JdiFrame,
    evaluator: RuntimeEvaluator,
    sourceLookUp: SourceLookUpProvider,
    override implicit val logger: Logger
) extends RuntimeDefaultValidator(frame, sourceLookUp, logger) {
  private def preEvaluate(tree: RuntimeEvaluableTree): Validation[PreEvaluatedTree] = {
    val value = evaluator.evaluate(tree)
    var tpe = value.extract(_.value.`type`)
    Validation.fromTry(tpe).map(PreEvaluatedTree(value, _))
  }

  override def validateLiteral(lit: Lit): Validation[RuntimeEvaluableTree] =
    super.validateLiteral(lit).flatMap(preEvaluate)

  override def localVarTreeByName(name: String, preevaluate: Boolean = true): Validation[RuntimeEvaluableTree] =
    super.localVarTreeByName(name).transform {
      case Valid(tree) if preevaluate => preEvaluate(tree)
      case tree => tree
    }

  override def fieldTreeByName(
      of: RuntimeTree,
      name: String,
      preevaluate: Boolean = true
  ): Validation[RuntimeEvaluableTree] =
    super.fieldTreeByName(of, name).transform {
      case tree if !preevaluate => tree
      case Valid(tree @ (_: StaticFieldTree | InstanceFieldTree(_, _: PreEvaluatedTree))) =>
        preEvaluate(tree)
      case Valid(tree @ (_: TopLevelModuleTree | NestedModuleTree(_, InstanceMethodTree(_, _, _: PreEvaluatedTree)))) =>
        preEvaluate(tree)
      case tree => tree
    }

  override def validateModule(name: String, of: Option[RuntimeTree]): Validation[RuntimeEvaluableTree] =
    super.validateModule(name, of).transform {
      case Valid(tree @ (_: TopLevelModuleTree | NestedModuleTree(_, InstanceMethodTree(_, _, _: PreEvaluatedTree)))) =>
        preEvaluate(tree)
      case tree => tree
    }

  override def validateOuter(tree: RuntimeTree): Validation[RuntimeEvaluableTree] =
    super.validateOuter(tree).transform {
      case Valid(tree @ (_: FieldTree | _: TopLevelModuleTree)) =>
        preEvaluate(tree)
      case tree => tree
    }

  override def validateIf(tree: Term.If): Validation[RuntimeEvaluableTree] =
    super.validateIf(tree).transform {
      case tree @ Valid(IfTree(p: PreEvaluatedTree, thenp, elsep, _)) =>
        val predicate = for {
          pValue <- p.value
          unboxed <- pValue.unboxIfPrimitive
          bool <- unboxed.toBoolean
        } yield bool
        predicate.extract match {
          case Success(true) => Valid(thenp)
          case Success(false) => Valid(elsep)
          case _ => tree
        }
      case tree => tree
    }
}

object RuntimePreEvaluationValidator {
  def apply(
      frame: JdiFrame,
      evaluator: RuntimeEvaluator,
      sourceLookup: SourceLookUpProvider,
      logger: Logger
  ): RuntimePreEvaluationValidator =
    new RuntimePreEvaluationValidator(frame, evaluator, sourceLookup, logger)
}
