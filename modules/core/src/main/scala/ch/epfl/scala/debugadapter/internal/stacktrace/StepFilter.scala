package ch.epfl.scala.debugadapter.internal.stacktrace

import com.sun.jdi.Method
import com.sun.jdi.Location

trait StepFilter {
  def skipOver(method: Method): Boolean = false
  def skipOut(upperLocation: Location, method: Method): Boolean = false
}
