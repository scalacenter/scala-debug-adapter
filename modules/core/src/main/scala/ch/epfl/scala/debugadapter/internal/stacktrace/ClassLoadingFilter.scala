package ch.epfl.scala.debugadapter.internal.stacktrace

import com.sun.jdi.{Location, Method}
import ch.epfl.scala.debugadapter.internal.ByteCodes

private[internal] object ClassLoadingFilter extends StepFilter {
  override def shouldSkipOut(upperLocation: Location, method: Method): Boolean = {
    val previousByteCode = upperLocation.method.bytecodes.apply(upperLocation.codeIndex.toInt)
    previousByteCode == ByteCodes.NEW && method.name != "<init>"
  }
}
