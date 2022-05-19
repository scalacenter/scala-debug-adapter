package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.transform.SymUtils.*
import dotty.tools.dotc.core.Types.*

/**
 * At this stage of the compilation, the `Expression` class contains a method with a name `evaluate` and a return type `Any`.
 * The goal of this phase is to change the return type of the `evaluate` method
 * and to update the owner and types of the symDenotations inserted into `evaluate`.
 */
class AdaptExpression(using
    evalCtx: EvaluationContext
)(using Context)
    extends MiniPhase
    with DenotTransformer:
  private var evaluateMethodSymbol: Symbol = _

  override def phaseName: String = AdaptExpression.name

  override def transform(ref: SingleDenotation)(using
      Context
  ): SingleDenotation =
    ref match
      case ref: SymDenotation
          if ref.name == evalCtx.evaluateName && ref.owner == evalCtx.expressionClass =>
        // set return type of the `evaluate` method to the return type of the expression
        // this is only useful to avoid boxing of primitive types
        if evalCtx.expressionType.typeSymbol.isPrimitiveValueClass then
          val info = MethodType(Nil)(_ => Nil, _ => evalCtx.expressionType)
          ref.copySymDenotation(info = info)
        else ref
      case ref: SymDenotation if ref.maybeOwner == evalCtx.expressionSymbol =>
        // update owner of the symDenotation, e.g. local vals
        // after it was inserted to `evaluate` method
        ref.copySymDenotation(owner = evalCtx.evaluateMethod)
      case ref: SymDenotation if evalCtx.nestedMethods.contains(ref) =>
        // update owner and type of the nested method
        val info = ref.info.asInstanceOf[MethodType]
        val paramsInfos =
          info.paramInfos.map { info =>
            if isTypeAccessible(info.typeSymbol.asType) then info
            else defn.ObjectType
          }
        val resType =
          if isTypeAccessible(info.resType.typeSymbol.asType) then info.resType
          else defn.ObjectType

        ref.copySymDenotation(
          owner = evalCtx.evaluateMethod,
          info = MethodType(info.paramNames)(_ => paramsInfos, _ => resType)
        )
      case _ =>
        ref

  // Check if a type is accessible from the expression class
  private def isTypeAccessible(symbol: TypeSymbol)(using Context): Boolean =
    !symbol.isPrivate && symbol.isAccessibleFrom(
      evalCtx.expressionClass.thisType
    ) && !symbol.isLocal

object AdaptExpression:
  val name: String = "adapt-expression"
