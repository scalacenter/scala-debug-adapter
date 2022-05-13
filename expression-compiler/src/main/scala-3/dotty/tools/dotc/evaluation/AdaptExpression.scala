package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.Symbols.Symbol

/**
 * At this stage of the compilation, the `Expression` class contains a method with a name `evaluate` and a return type `Any`.
 * The goal of this phase is to change the return type of the `evaluate` method
 * and to update the owner of the extracted symDenotations after they were inserted into the `evaluate` method.
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
          if ref.owner.name.toString == evalCtx.expressionClassName && ref.name.toString == "evaluate" =>
        // set return type of the `evaluate` method to the return type of the expression
        if evalCtx.expressionType.typeSymbol.isPublic then
          ref.copySymDenotation(info = evalCtx.expressionType)
        else ref
      case ref: SymDenotation if ref.maybeOwner == evalCtx.expressionSymbol =>
        // update owner of the symDenotation
        // after it was inserted to `evaluate` method
        ref.copySymDenotation(owner = evalCtx.evaluateMethod)
      case ref: SymDenotation if evalCtx.nestedMethods.contains(ref) =>
        // update owner of the nested methods,
        // after they were inserted to `evaluate` method
        ref.copySymDenotation(owner = evalCtx.evaluateMethod)
      case _ =>
        ref

object AdaptExpression:
  val name: String = "adapt-expression"
