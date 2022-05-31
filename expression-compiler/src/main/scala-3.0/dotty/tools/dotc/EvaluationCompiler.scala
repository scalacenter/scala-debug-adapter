package dotty.tools.dotc

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.evaluation.*
import dotty.tools.dotc.util.SourceFile

class EvaluationCompiler(
    sourceFile: SourceFile
)(using EvaluationContext)(using Context)
    extends Compiler:

  override protected def frontendPhases: List[List[Phase]] =
    val frontEnd :: others = super.frontendPhases
    List(EvaluationFrontEnd()) ::
      List(PrepareExtractExpression()) ::
      List(ExtractExpression()) ::
      others

  override protected def picklerPhases: List[List[Phase]] = List()

  override protected def transformPhases: List[List[Phase]] =
    super.transformPhases :+ List(ResolveReflectEval())
