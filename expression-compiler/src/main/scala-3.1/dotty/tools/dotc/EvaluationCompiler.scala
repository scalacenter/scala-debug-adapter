package dotty.tools.dotc

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.evaluation.*

class EvaluationCompiler(using EvaluationContext)(using Context)
    extends Compiler:

  override protected def frontendPhases: List[List[Phase]] =
    val parser :: others = super.frontendPhases
    parser :: List(InsertExpression()) :: others

  override protected def picklerPhases: List[List[Phase]] =
    super.picklerPhases :+ List(ExtractExpression())

  override protected def transformPhases: List[List[Phase]] =
    super.transformPhases :+ List(ResolveReflectEval())
