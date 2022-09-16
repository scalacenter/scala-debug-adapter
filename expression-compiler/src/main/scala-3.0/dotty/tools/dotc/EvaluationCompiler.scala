package dotty.tools.dotc

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.evaluation.*
import dotty.tools.dotc.util.SourceFile

class EvaluationCompiler(using EvaluationContext)(using Context) extends Compiler:

  override protected def frontendPhases: List[List[Phase]] =
    List(EvaluationFrontEnd()) :: super.frontendPhases.tail

  override protected def picklerPhases: List[List[Phase]] =
    super.picklerPhases :+ List(ExtractExpression())

  override protected def transformPhases: List[List[Phase]] =
    super.transformPhases :+ List(ResolveReflectEval())
