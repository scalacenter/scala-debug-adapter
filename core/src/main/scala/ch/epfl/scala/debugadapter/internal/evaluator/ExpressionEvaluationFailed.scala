package ch.epfl.scala.debugadapter.internal.evaluator

private[internal] class ExpressionEvaluationFailed(message: String)
    extends Exception(message)
