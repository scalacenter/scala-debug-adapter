package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.ObjectReference

private[internal] case class MethodInvocationFailed(
    message: String,
    remoteException: ObjectReference
) extends Exception(message)
