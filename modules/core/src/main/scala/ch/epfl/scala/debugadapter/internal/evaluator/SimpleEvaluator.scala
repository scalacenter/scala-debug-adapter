package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import com.sun.jdi.*

import scala.jdk.CollectionConverters.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class SimpleEvaluator(logger: Logger, testMode: Boolean) {
  def prepare(expression: String, frame: JdiFrame): Option[LocalValue] = {
    val encodedExpression = NameTransformer.encode(expression)
    if (isLocalVariable(frame, encodedExpression)) Some(LocalValue(encodedExpression))
    else None
  }

  def evaluate(localValue: LocalValue, frame: JdiFrame): Try[Value] = Try {
    val currentFrame = frame.current()
    val variable = currentFrame.visibleVariableByName(localValue.name)
    val rawValue = currentFrame.getValue(variable)
    JdiValue(rawValue, frame.thread).derefIfRef.value
  }

  private def isLocalVariable(frame: JdiFrame, name: String): Boolean = {
    Try(frame.current().visibleVariables.asScala.toList) match {
      case Success(localVariables) =>
        // we exclude the arguments of type scala.Function0
        // they could be by-name params, in which case we should invoke apply
        // but they could also be just normal params and we have no way to know
        localVariables
          .find(_.name == name)
          .exists(v => !v.isArgument || v.typeName != "scala.Function0")
      case Failure(exception) =>
        throwOrWarn("Cannot get local variables", exception)
        false
    }
  }

  private def throwOrWarn(msg: String, throwable: Throwable): Unit = {
    val message = s"$msg because ${throwable.getClass.getSimpleName}: ${throwable.getMessage}"
    if (testMode) throw new Exception(message, throwable)
    else logger.warn(message)
  }
}
