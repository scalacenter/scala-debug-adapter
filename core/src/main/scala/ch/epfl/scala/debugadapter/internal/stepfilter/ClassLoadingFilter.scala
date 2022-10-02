package ch.epfl.scala.debugadapter.internal.stepfilter

import com.sun.jdi.{Location, Method}
import ch.epfl.scala.debugadapter.internal.ByteCodes

object ClassLoadingStepFilter extends StepFilter {
  override def shouldStepOut(previousLocation: Location, method: Method): Boolean = {
    val previousByteCode = previousLocation.method.bytecodes.apply(previousLocation.codeIndex.toInt)
    previousByteCode == ByteCodes.NEW && method.name != "<init>"
  }
}
