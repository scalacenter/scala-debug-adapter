package ch.epfl.scala.debugadapter.internal.evaluator

private[internal] case class DebuggeeInvocationException(
    message: String,
    remoteException: Option[JdiObject]
) extends Exception(message)
