package ch.epfl.scala.debugadapter.internal.stepfilter
import com.sun.jdi.Method
import com.sun.jdi.Location
trait StepFilter {
  def shouldSkipOver(method: Method): Boolean = false
  def shouldSkipOut(upperLocation: Location, method: Method): Boolean = false
}
