package ch.epfl.scala.debugadapter.internal.stepfilter

import com.sun.jdi.Method

trait StepFilter {
  def skip(method: Method): Boolean
}
