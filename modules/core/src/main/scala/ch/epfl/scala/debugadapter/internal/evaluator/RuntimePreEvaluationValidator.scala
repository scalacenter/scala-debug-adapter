package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger

class RuntimePreEvaluationValidator(
    override val frame: JdiFrame,
    override val logger: Logger,
    evaluator: RuntimeEvaluator
) extends RuntimeDefaultValidator(frame, logger) {
  private def preEvaluate(tree: RuntimeEvaluableTree): Validation[PreEvaluatedTree] = {
    val value = evaluator.evaluate(tree)
    var tpe = value.extract(_.value.`type`)
    Validation.fromTry(tpe).map(PreEvaluatedTree(value, _))
  }

  override lazy val thisTree: Validation[PreEvaluatedTree] =
    ThisTree(frame.thisObject).flatMap(preEvaluate)

  override def localVarTreeByName(name: String): Validation[PreEvaluatedTree] =
    super.localVarTreeByName(name).flatMap(preEvaluate)

  override def fieldTreeByName(of: Validation[RuntimeTree], name: String): Validation[RuntimeEvaluableTree] =
    super.fieldTreeByName(of, name).flatMap {
      case tree @ (_: StaticFieldTree | InstanceFieldTree(_, _: PreEvaluatedTree)) =>
        preEvaluate(tree)
      case tree @ (_: TopLevelModuleTree | NestedModuleTree(_, _: PreEvaluatedTree)) =>
        preEvaluate(tree)
      case tree => Valid(tree)
    }

  override def validateModule(name: String, of: Option[RuntimeTree]): Validation[RuntimeEvaluableTree] =
    super.validateModule(name, of).flatMap {
      case tree @ (_: TopLevelModuleTree | NestedModuleTree(_, _: PreEvaluatedTree)) =>
        preEvaluate(tree)
      case tree => Valid(tree)
    }

  override def findOuter(tree: RuntimeTree): Validation[RuntimeEvaluableTree] =
    super.findOuter(tree).flatMap {
      case tree @ (_: OuterModuleTree | OuterClassTree(_: PreEvaluatedTree, _)) =>
        preEvaluate(tree)
      case tree => Valid(tree)
    }
}

object RuntimePreEvaluationValidator {
  def apply(frame: JdiFrame, logger: Logger, evaluator: RuntimeEvaluator): RuntimePreEvaluationValidator =
    new RuntimePreEvaluationValidator(frame, logger, evaluator)
}
