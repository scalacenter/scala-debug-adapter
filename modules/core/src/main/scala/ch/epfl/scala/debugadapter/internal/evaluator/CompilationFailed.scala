package ch.epfl.scala.debugadapter.internal.evaluator

private[internal] class CompilationFailed(errors: Seq[String]) extends Exception(errors.mkString("\n")) {

  override def toString(): String = {
    s"failed compilation:\n$getMessage"
  }
}
