package ch.epfl.scala.debugadapter.internal.stacktrace

import com.sun.jdi.{Location, Method}
import ch.epfl.scala.debugadapter.internal.ByteCode

private[internal] object ClassLoadingFilter extends StepFilter {
  val classLoadingCodes = Set(ByteCode.NEW, ByteCode.ANEWARRAY, ByteCode.MULTIANEWARRAY, ByteCode.LDC)
  override def shouldSkipOut(upperLocation: Location, method: Method): Boolean = {
    val previousByteCode = upperLocation.method.bytecodes.apply(upperLocation.codeIndex.toInt)
    classLoadingCodes.contains(previousByteCode) && method.name != "<init>"
  }
}
