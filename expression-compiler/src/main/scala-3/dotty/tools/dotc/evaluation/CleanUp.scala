package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

class CleanUp(using evalCtx: EvaluationContext) extends MiniPhase:
  override def phaseName: String = CleanUp.name

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    if tree.name.toString != evalCtx.expressionClassName then EmptyTree
    else tree
end CleanUp

object CleanUp:
  val name: String = "cleanup"
end CleanUp
