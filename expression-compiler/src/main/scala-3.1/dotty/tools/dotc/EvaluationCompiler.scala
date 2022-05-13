package dotty.tools.dotc

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.evaluation.AdaptExpression
import dotty.tools.dotc.evaluation.CleanUp
import dotty.tools.dotc.evaluation.ExtractDefs
import dotty.tools.dotc.evaluation.ExtractExpression
import dotty.tools.dotc.evaluation.InsertExpression
import dotty.tools.dotc.evaluation.InsertExtracted
import dotty.tools.dotc.transform.LambdaLift
import dotty.tools.dotc.util.SourceFile

class EvaluationCompiler(
    sourceFile: SourceFile,
    expressionClassName: String,
    breakpointLine: Int,
    expression: String,
    defNames: Set[String]
)(using Context)
    extends Compiler:
  private given evalCtx: EvaluationContext = EvaluationContext(
    expressionClassName,
    breakpointLine,
    expression,
    defNames
  )

  override protected def frontendPhases: List[List[Phase]] =
    val parser :: typer :: others = super.frontendPhases
    parser
      :: List(InsertExpression())
      :: typer
      :: List(ExtractExpression())
      :: List(ExtractDefs())
      :: List(CleanUp())
      :: List(InsertExtracted())
      :: List(AdaptExpression())
      :: others

  override protected def picklerPhases: List[List[Phase]] = List()
end EvaluationCompiler
