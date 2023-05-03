package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._

private[internal] class JdiString(ref: StringReference, thread: ThreadReference) extends JdiValue(ref, thread) {
  def stringValue: String = ref.value
}
