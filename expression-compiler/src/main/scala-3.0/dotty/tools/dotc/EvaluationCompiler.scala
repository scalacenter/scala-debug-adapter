package dotty.tools.dotc

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.evaluation.AdaptExpression
import dotty.tools.dotc.evaluation.CleanUp
import dotty.tools.dotc.evaluation.EvaluationFrontEnd
import dotty.tools.dotc.evaluation.ExtractDefs
import dotty.tools.dotc.evaluation.ExtractExpression
import dotty.tools.dotc.evaluation.InsertExpression
import dotty.tools.dotc.evaluation.InsertExtracted
import dotty.tools.dotc.transform.LambdaLift
import dotty.tools.dotc.util.SourceFile

class EvaluationCompiler(
    sourceFile: SourceFile,
    expressionClassName: String,
    valuesByNameIdentName: String,
    breakpointLine: Int,
    expression: String,
    defNames: Set[String]
)(using Context)
    extends Compiler:
  private given evalCtx: EvaluationContext = EvaluationContext(
    sourceFile,
    expressionClassName,
    breakpointLine,
    expression,
    defNames
  )

  override protected def frontendPhases: List[List[Phase]] =
    val frontEnd :: other = super.frontendPhases
    List(EvaluationFrontEnd())
      :: List(ExtractExpression())
      :: List(ExtractDefs())
      :: List(InsertExtracted())
      :: List(AdaptExpression())
      :: List(CleanUp())
      :: other

  override protected def picklerPhases: List[List[Phase]] = List()
end EvaluationCompiler
