package scala.tools.nsc

import scala.tools.nsc.reporters.Reporter

private[nsc] class EvaluationGlobal(
    settings: Settings,
    reporter: Reporter,
    val line: Int,
    val expression: String,
    val localVariables: Set[String],
    val expressionClassName: String
) extends Global(settings, reporter) {
  override protected def computeInternalPhases(): Unit = {
    super.computeInternalPhases()

    addToPhasesSet(
      new InsertExpression(this),
      "Insert expression which is going to be evaluated"
    )
    addToPhasesSet(
      new GenerateExpression(this),
      "Generate the final form of the expression"
    )
  }

  case object ExpressionAttachment extends PlainAttachment
}
