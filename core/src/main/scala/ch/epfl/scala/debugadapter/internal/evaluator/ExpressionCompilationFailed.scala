package ch.epfl.scala.debugadapter.internal.evaluator

private[internal] class ExpressionCompilationFailed(message: String)
    extends Exception(message)
