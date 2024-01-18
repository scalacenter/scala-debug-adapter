package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.SourceLookUpProvider
import com.sun.jdi.Value

import scala.util.Failure
import scala.util.Success
import scala.util.Try

class RuntimeEvaluator(sourceLookUp: SourceLookUpProvider, logger: Logger) {
  def validate(expression: String, frame: JdiFrame, preEvaluation: Boolean): Try[RuntimeExpression] = {
    val validation = new RuntimeValidation(frame, sourceLookUp, preEvaluation)(logger)
    validation.validate(expression) match {
      case invalid: Invalid => Failure(invalid.exception)
      case Valid(expr) => Success(RuntimeExpression(expr))
    }
  }

  def evaluate(expression: RuntimeExpression, frame: JdiFrame): Try[Value] = {
    val evaluation = new RuntimeEvaluation(frame, logger)
    evaluation.evaluate(expression.tree).getResult.map(_.value)
  }
}
