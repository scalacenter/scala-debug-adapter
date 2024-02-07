package ch.epfl.scala.debugadapter.internal.evaluator

private[internal] case class MethodInvocationFailed(
    message: String,
    remoteException: Option[JdiObject]
) extends Exception(message) {
  override def toString: String = s"MethodInvocationFailed: $message"
}
