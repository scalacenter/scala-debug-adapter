package ch.epfl.scala.debugadapter.internal.evaluator

private[internal] case class RuntimeException(
    message: String,
    remoteException: Option[JdiObject]
) extends Exception(message) {
  override def toString: String = s"RuntimeException: $message"
}
