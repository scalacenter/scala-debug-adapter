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
    if tree.name.toString == evalCtx.evaluationClassName && tree.symbol.isClass
    then evalCtx.evaluationClass = tree.symbol.asClass
    tree

object ExtractDefs:
  val name: String = "extract-defs"
