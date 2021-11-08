package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols.TermSymbol
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.transform.SymUtils.isField

class InsertExtracted(using evalCtx: EvaluationContext) extends MiniPhase:
  thisPhase =>
  override def phaseName: String = InsertExtracted.name

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    if tree.name.toString == "evaluate" then
      evalCtx.evaluateMethod = tree.symbol
      val transformedExpression = ExpressionTransformer().transform(
        evalCtx.expressionValDef
      )
      val transformedNestedMethods = evalCtx.nestedMethods.values
        .map(nestedMethod => ExpressionTransformer().transform(nestedMethod))
        .toList
      val expressionBlock =
        Block(
          List(transformedExpression),
          evalCtx.expressionIdent
        )
      DefDef(
        tree.symbol.asInstanceOf[TermSymbol],
        List(),
        tree.tpt.tpe,
        Block(transformedNestedMethods, expressionBlock)
      )
    else super.transformDefDef(tree)

  class ExpressionTransformer extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case tree @ This(Ident(name)) =>
          val thisOrOuter =
            if tree.symbol == evalCtx.originalThis then "$this" else "$outer"
          if (evalCtx.defTypes.contains(thisOrOuter)) mkIdent(thisOrOuter)
          else super.transform(tree)
        case tree @ Select(This(_), name)
            if tree.qualifier.symbol == evalCtx.originalThis && tree.symbol.isField =>
          mkIdent(name.toString)
        case Ident(name)
            if evalCtx.defTypes
              .contains(name.toString) && evalCtx.defTypes.contains(
              name.toString
            ) =>
          mkIdent(name.toString)
        case tree: Apply
            if tree.fun.isInstanceOf[Select] && tree.fun.symbol.isPrivate =>
          val privateCall = mkCallPrivate(tree)
          super.transform(privateCall)
        case _ =>
          super.transform(tree)
  end ExpressionTransformer
end InsertExtracted

object InsertExtracted:
  val name: String = "eval-insertExtracted"
end InsertExtracted
