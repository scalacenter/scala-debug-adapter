package ch.epfl.scala.debugadapter.internal.evaluator

private[internal] class ExpressionCompilationFailed(message: String)
    extends Exception(message) {

  override def toString(): String = {
    s"""|compiler error:
        |  $message""".stripMargin
  }
}
