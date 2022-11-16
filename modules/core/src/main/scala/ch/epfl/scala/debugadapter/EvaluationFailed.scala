package ch.epfl.scala.debugadapter

private[debugadapter] class EvaluationFailed(msg: String) extends Exception(msg) {

  override def toString(): String = {
    s"failed evaluation:\n$msg"
  }
}
