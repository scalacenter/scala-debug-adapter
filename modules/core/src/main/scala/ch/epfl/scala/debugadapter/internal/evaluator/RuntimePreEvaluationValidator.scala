package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.evaluator.RuntimeDefaultEvaluator

class RuntimePreEvaluationValidator(frame: JdiFrame, logger: Logger, evaluator: RuntimeDefaultEvaluator)
    extends RuntimeDefaultValidator(frame, logger) {
  override def localVarTreeByName(name: String): Validation[PreEvaluatedTree] = {
    super.localVarTreeByName(name).map { localVar => PreEvaluatedTree(evaluator.evaluate(localVar), localVar.`type`) }
  }

  override def fieldTreeByName(of: Validation[RuntimeTree], name: String): Validation[RuntimeEvaluableTree] =
    super.fieldTreeByName(of, name).map { field =>
      of match {
        case Valid(_: PreEvaluatedTree) => PreEvaluatedTree(evaluator.evaluate(field), field.`type`)
        case _ => field
      }
    }

  override def validateMethod(call: Call): Validation[RuntimeEvaluableTree] =
    super.validateMethod(call).map {
      case binary: PrimitiveBinaryOpTree => PreEvaluatedTree(evaluator.evaluate(binary), binary.`type`)
      case unary: PrimitiveUnaryOpTree => PreEvaluatedTree(evaluator.evaluate(unary), unary.`type`)
      case method => method
    }

  // override def validateModule(name: String, of: Option[RuntimeTree]): Validation[RuntimeEvaluableTree] =
  //   super.validateModule(name, of).map {
  //     case mod: TopLevelModuleTree => PreEvaluatedTree(evaluator.evaluateModule(mod), mod.`type`)
  //     case NestedModuleTree(modCls, of: PreEvaluatedTree) =>
  //       val module = for {
  //         parent <- of.value
  //         loader <- frame.classLoader()
  //         initMethodName <- Safe(Helpers.getLastInnerType(modCls.name()).get)
  //         instance <- parent.value.`type`() match {
  //           case module if module == modCls => of.value
  //           case _ => parent.asObject.invoke(initMethodName, Seq.empty)
  //         }
  //       } yield instance
  //       PreEvaluatedTree(module, modCls)
  //   }

}
