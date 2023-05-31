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
  override def localVarTreeByName(name: String): Validation[PreEvaluatedTree] =
    for {
      localVar <- super.localVarTreeByName(name)
      extracted <- extractFrom(localVar)
    } yield PreEvaluatedTree(extracted)

  override def fieldTreeByName(of: Validation[RuntimeTree], name: String): Validation[RuntimeEvaluableTree] =
    of match {
      case Valid(evaluated: PreEvaluatedTree) =>
        for {
          field <- super.fieldTreeByName(Valid(evaluated), name)
          extracted <- extractFrom(field)
        } yield PreEvaluatedTree(extracted)
      case _ => super.fieldTreeByName(of, name)
    }

  // TODO: find how to return a make validateModule returns a ModuleTree...
  override def validateModule(name: String, of: Option[RuntimeTree]): Validation[RuntimeEvaluableTree] =
    super.validateModule(name, of) flatMap {
      case mod: ModuleTree => extractFrom(mod).map(PreEvaluatedTree(_))
      case module => Valid(module)
    }

}
