package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.transform.SymUtils.isField
import dotty.tools.dotc.core.Flags.*

/**
 * This phase is responsible for extracting types of definitions (`DefDef` and `ValDef`) and anonymous functions.
 */
class ExtractDefs(using evalCtx: EvaluationContext) extends MiniPhase:
  override def phaseName: String = ExtractDefs.name

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    val isExpressionClass =
      tree.name.toString == evalCtx.expressionClassName && tree.symbol.isClass
    if isExpressionClass then evalCtx.expressionClass = tree.symbol.asClass
    super.transformTypeDef(tree)

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    // extract nested methods
    if isCorrectOwner(tree) && tree.symbol.owner.is(Method) then
      evalCtx.nestedMethods += tree.symbol.denot -> tree
    super.transformDefDef(tree)

  private def isCorrectOwner(tree: Tree)(using Context): Boolean =
    evalCtx.expressionOwners.contains(tree.symbol.maybeOwner)

object ExtractDefs:
  val name: String = "extract-defs"
