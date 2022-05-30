package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.core.Flags.*

/**
 * This phase is responsible for extracting informations related to expression,
 * such as expression itself, its owners and orginal `this` symbol.
 */
class ExtractExpression(using evalCtx: EvaluationContext) extends MiniPhase:
  override def phaseName: String = ExtractExpression.name

  override def transformValDef(tree: ValDef)(using Context): Tree =
    if tree.name == evalCtx.expressionTermName && evalCtx.expressionTree == null
    then
      evalCtx.originalThis =
        tree.symbol.ownersIterator.find(_.isClass).get.asClass
      evalCtx.expressionTree = tree.rhs
      evalCtx.expressionSymbol = tree.symbol
      evalCtx.expressionType = tree.tpe

      evalCtx.classOwners = tree.symbol.ownersIterator.collect {
        case cls: ClassSymbol => cls
      }.toSeq
      evalCtx.capturingMethod = tree.symbol.ownersIterator
        .find(sym => (sym.isClass || sym.is(Method)) && sym.owner.is(Method))
        .collect { case sym if sym.isTerm => sym.asTerm }

      unitLiteral
    else super.transformValDef(tree)

object ExtractExpression:
  val name: String = "extract-expression"
