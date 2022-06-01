package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

class PrepareExtractExpression(using evalCtx: EvaluationContext)
    extends MiniPhase:
  override def phaseName: String = PrepareExtractExpression.name

  override def transformValDef(tree: ValDef)(using Context): Tree =
    if tree.name == evalCtx.expressionTermName
    then
      evalCtx.expressionSymbol = tree.symbol.asTerm

      evalCtx.classOwners = tree.symbol.ownersIterator.collect {
        case cls: ClassSymbol => cls
      }.toSeq
      evalCtx.capturingMethod = tree.symbol.ownersIterator
        .find(sym => (sym.isClass || sym.is(Method)) && sym.owner.is(Method))
        .collect { case sym if sym.isTerm => sym.asTerm }

      unitLiteral
    else super.transformValDef(tree)

object PrepareExtractExpression:
  val name: String = "prepare-extract-expression"
