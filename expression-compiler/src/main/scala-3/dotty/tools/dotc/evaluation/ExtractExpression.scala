package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols.ClassSymbol
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

/**
 * This phase is responsible for extracting informations related to expression,
 * such as expression itself, its owners and orginal `this` symbol.
 */
class ExtractExpression(using evalCtx: EvaluationContext) extends MiniPhase:
  override def phaseName: String = ExtractExpression.name

  override def transformValDef(tree: ValDef)(using Context): Tree =
    if tree.name == evalCtx.expressionTermName && evalCtx.expressionTree == null
    then
      evalCtx.expressionOwners = tree.symbol.ownersIterator.toList
      evalCtx.originalThis =
        tree.symbol.ownersIterator.find(_.isClass).get.asClass
      evalCtx.expressionTree = tree.rhs
      evalCtx.expressionSymbol = tree.symbol
      evalCtx.expressionType = tree.tpe
      tree
    else super.transformValDef(tree)

object ExtractExpression:
  val name: String = "extract-expression"
