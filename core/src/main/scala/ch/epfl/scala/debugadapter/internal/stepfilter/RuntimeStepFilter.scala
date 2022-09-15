package ch.epfl.scala.debugadapter.internal.stepfilter

import com.sun.jdi.Method
import ch.epfl.scala.debugadapter.DebuggeeRunner

private class RuntimeStepFilter(
    classesToSkip: Set[String],
    methodsToSkip: Set[String]
) extends StepFilter {
  override def skip(method: Method): Boolean =
    classesToSkip.contains(method.declaringType.name) ||
      methodsToSkip.contains(method.toString)
}

private[internal] object RuntimeStepFilter {
  private val javaClassesToSkip = Set("sun.misc.Unsafe")
  private val javaMethodsToSkip = Set(
    "java.lang.invoke.DirectMethodHandle.internalMemberName(java.lang.Object)"
  )
  private val scala3ClassesToSkip = Set("scala.runtime.LazyVals$")
  private val scala2ClassesToSkip = Set.empty[String]

  def apply(runner: DebuggeeRunner): StepFilter = {
    if (runner.scalaVersion.startsWith("2"))
      new RuntimeStepFilter(
        scala2ClassesToSkip ++ javaClassesToSkip,
        javaMethodsToSkip
      )
    else
      new RuntimeStepFilter(
        scala3ClassesToSkip ++ scala2ClassesToSkip ++ javaClassesToSkip,
        javaMethodsToSkip
      )
  }
}
