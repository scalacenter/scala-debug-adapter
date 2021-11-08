package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols.ClassSymbol
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

/**
 * This phase is responsible for extracting informations related to expression,
 * such as expression itself, its owners, type and orginal `this` symbol.
 */
class ExtractExpression(using evalCtx: EvaluationContext) extends MiniPhase:
  override def phaseName: String = ExtractExpression.name

  override def transformValDef(tree: ValDef)(using Context): Tree =
    if shouldExtractValDef(tree) then
      evalCtx.expressionOwners = tree.symbol.ownersIterator.toList
      evalCtx.expressionType = tree.tpe
      evalCtx.expressionValDef = tree
      evalCtx.originalThis = tree.symbol.ownersIterator.toList
        .find(_.isClass)
        .map(_.asInstanceOf[ClassSymbol])
        .get

      evalCtx.defTypes += "$this" -> evalCtx.originalThis.thisType
      evalCtx.originalThis.ownersIterator
        .find(owner =>
          owner != evalCtx.originalThis && owner.isClass && !owner.isEmptyPackage && !owner.isRoot
        )
        .map(_.thisType)
        .foreach(outerType => evalCtx.defTypes += "$outer" -> outerType)
    super.transformValDef(tree)

  override def transformIdent(tree: Ident)(using Context): Tree =
    if shouldExtractIdent(tree) then evalCtx.expressionIdent = tree
    super.transformIdent(tree)

  // TODO: check expression owner
  private def shouldExtractValDef(tree: ValDef)(using Context): Boolean =
    tree.name.toString == evalCtx.expressionIdentName && evalCtx.expressionValDef == null

  // TODO: check expression owner
  private def shouldExtractIdent(tree: Ident)(using Context): Boolean =
    tree.name.toString == evalCtx.expressionIdentName && evalCtx.expressionIdent == null
end ExtractExpression

object ExtractExpression:
  val name: String = "eval-extractExpression"
end ExtractExpression
