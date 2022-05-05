package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.Symbols.Symbol

/**
 * At this stage of the compilation, the `Expression` class contains a method with a name `evaluate` and a return type `Unit`.
 * Its goal is to return a value of an expression that is being evaluated. This phase changes a return type of a method `evaluate`
 * to the type of that expression.
 *
 * The second goal of this phase is to update owner of the extracted expression and extrated nested methods
 * after they were inserted to `evalaute` method.
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
      case ref: SymDenotation if isEvaluateMethod(ref) =>
        // set return type of the `evaluate` method to the return type of the expression
        ref.copySymDenotation(info = evalCtx.expressionType)
      case ref: SymDenotation
          if ref.name.toString == evalCtx.expressionIdentName =>
        // update owner of the extracted expression,
        // after it was inserted to `evaluate` method
        ref.copySymDenotation(owner = evalCtx.evaluateMethod)
      case ref: SymDenotation if evalCtx.nestedMethods.contains(ref) =>
        // update owner of the nested methods,
        // after they were inserted to `evaluate` method
        ref.copySymDenotation(owner = evalCtx.evaluateMethod)
      case _ =>
        ref

  private def isEvaluateMethod(ref: SymDenotation)(using Context): Boolean =
    ref.isRealMethod && ref.owner.name.toString == evalCtx.expressionClassName && ref.name.toString == "evaluate"
end AdaptExpression

object AdaptExpression:
  val name: String = "adapt-expression"
end AdaptExpression
