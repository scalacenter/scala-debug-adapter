package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.ThreadReference
import com.sun.jdi.StackFrame

final case class FrameReference(thread: ThreadReference, depth: Int) {
  def current(): StackFrame = thread.frame(depth)
}
