package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._

private[evaluator] class JdiObject(val reference: ObjectReference, thread: ThreadReference) {
  def invoke(methodName: String, args: List[Value]): Option[Value] = for {
    method <- method(methodName, reference.referenceType())
    result <- invokeMethod(reference, method, args, thread)
  } yield result

  def invoke(methodName: String, signature: String, args: List[Value]): Option[Value] = for {
    method <- method(methodName, signature, reference.referenceType())
    result <- invokeMethod(reference, method, args, thread)
  } yield result
}
