package ch.epfl.scala.debugadapter.internal.evaluator

private[internal] case class MethodInvocationFailed(
    message: String,
    remoteException: JdiObject
) extends Exception(message)
