package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import com.sun.jdi.Type

class RuntimePreEvaluationValidator(frame: JdiFrame, logger: Logger, evaluator: RuntimeDefaultEvaluator)
    extends RuntimeDefaultValidator(frame, logger) {
  private def extractFrom(tree: RuntimeEvaluableTree): Validation[(Safe[JdiValue], Type)] = {
    val value = evaluator.evaluate(tree)
    var tpe = value.extract(_.value.`type`)
    Validation.fromTry(tpe).map((value, _))
  }

  override lazy val thisTree: Validation[PreEvaluatedTree] =
    Validation.fromOption(frame.thisObject).map(ths => PreEvaluatedTree(Safe(ths), ths.reference.referenceType()))

  override def localVarTreeByName(name: String): Validation[PreEvaluatedTree] =
    super.localVarTreeByName(name).flatMap(extractFrom).map(PreEvaluatedTree(_))

  override def fieldTreeByName(of: Validation[RuntimeTree], name: String): Validation[RuntimeEvaluableTree] =
    of match {
      case Valid(evaluated: PreEvaluatedTree) =>
        super
          .fieldTreeByName(Valid(evaluated), name)
          .flatMap(extractFrom)
          .map(PreEvaluatedTree(_))
      case _ => super.fieldTreeByName(of, name)
    }

  override def validateModule(name: String, of: Option[RuntimeTree]): Validation[RuntimeEvaluableTree] =
    of match {
      case None | Some(_: PreEvaluatedTree) =>
        super
          .validateModule(name, of)
          .flatMap(extractFrom)
          .map(PreEvaluatedTree(_))
      case _ => super.validateModule(name, of)
    }
}
