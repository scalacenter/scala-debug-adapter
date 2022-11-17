package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._

private[internal] class JdiObject(
    val reference: ObjectReference,
    thread: ThreadReference
) {
  def getFieldValue(name: String): Value = {
    val field = reference.referenceType.fieldByName(name)
    reference.getValue(field)
  }

  def invoke(methodName: String, args: List[Value]): Safe[Value] = {
    val m = method(methodName, reference.referenceType)
    invokeMethod(reference, m, args, thread)
  }

  def invoke(methodName: String, signature: String, args: List[Value]): Safe[Value] = {
    val m = method(methodName, signature, reference.referenceType())
    invokeMethod(reference, m, args, thread)
  }
}
