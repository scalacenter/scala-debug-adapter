package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols.ClassSymbol
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.transform.SymUtils.isField
import dotty.tools.dotc.core.Flags

/**
 * This phase is responsible for extracting types of definitions (`DefDef` and `ValDef`) and anonymous functions.
 */
class ExtractDefs(using evalCtx: EvaluationContext) extends MiniPhase:
  override def phaseName: String = ExtractDefs.name

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    val isExpressionClass =
      tree.name.toString == evalCtx.expressionClassName && tree.symbol
        .isInstanceOf[ClassSymbol]
    if isExpressionClass then
      evalCtx.expressionThis = tree.symbol.asInstanceOf[ClassSymbol]
    super.transformTypeDef(tree)

  override def transformValDef(tree: ValDef)(using Context): Tree =
    if tree.symbol.isField && evalCtx.defNames.contains(tree.name.toString) then
      evalCtx.defTypes += (tree.name.toString -> tree.tpe)
    super.transformValDef(tree)

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    if evalCtx.expressionOwners.contains(tree.symbol.owner) && tree.symbol.owner
        .is(Flags.Method)
    then evalCtx.nestedMethods += tree.symbol.denot -> tree
    super.transformDefDef(tree)
end ExtractDefs

object ExtractDefs:
  val name: String = "eval-extractDefs"
end ExtractDefs
