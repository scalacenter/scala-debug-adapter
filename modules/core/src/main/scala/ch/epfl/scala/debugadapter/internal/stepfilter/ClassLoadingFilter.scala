package ch.epfl.scala.debugadapter.internal.stepfilter

import com.sun.jdi.{Location, Method}
import ch.epfl.scala.debugadapter.internal.ByteCodes

object ClassLoadingStepFilter extends StepFilter {

  override def shouldSkipOut(upperLocation: Location, method: Method): Boolean = {
    val previousByteCode = upperLocation.method.bytecodes.apply(upperLocation.codeIndex.toInt)
    previousByteCode == ByteCodes.NEW && method.name != "<init>"
  }
}
