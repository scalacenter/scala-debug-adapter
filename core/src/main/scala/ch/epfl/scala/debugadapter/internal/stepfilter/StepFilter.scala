package ch.epfl.scala.debugadapter.internal.stepfilter

import com.sun.jdi.Method
import com.sun.jdi.Location

trait StepFilter {
  def shouldStepInto(method: Method): Boolean = false
  def shouldStepOut(upperLocation: Location, method: Method): Boolean = false
}
