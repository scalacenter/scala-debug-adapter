package dotty.tools.dotc

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.evaluation.*
import dotty.tools.dotc.util.SourceFile

class EvaluationCompiler(
    sourceFile: SourceFile,
    expressionClassName: String,
    breakpointLine: Int,
    expression: String,
    defNames: Set[String]
)(using Context)
    extends Compiler:
  private given EvaluationContext = EvaluationContext(
    expressionClassName,
    breakpointLine,
    expression,
    defNames
  )

  override protected def frontendPhases: List[List[Phase]] =
    val parser :: typer :: others = super.frontendPhases
    parser ::
      List(InsertExpression()) ::
      typer ::
      List(ExtractExpression()) ::
      List(ExtractDefs()) ::
      List(InsertExtracted()) ::
      others

  override protected def picklerPhases: List[List[Phase]] = List()

  override protected def transformPhases: List[List[Phase]] =
    super.transformPhases :+ List(ResolveReflectEval())
