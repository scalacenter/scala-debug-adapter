package ch.epfl.scala.debugadapter.internal.evaluator

private[internal] class ExpressionCompilationFailed(errors: Seq[String]) extends Exception(errors.mkString("\n")) {

  override def toString(): String = {
    s"failed compilation:\n$getMessage"
  }
}
