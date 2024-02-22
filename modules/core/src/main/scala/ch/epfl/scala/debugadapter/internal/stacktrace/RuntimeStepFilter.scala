package ch.epfl.scala.debugadapter.internal.stacktrace

import com.sun.jdi.Method
import com.sun.jdi.Location
import ch.epfl.scala.debugadapter.ScalaVersion

private[internal] class RuntimeStepFilter(classesToSkip: Set[String], methodsToSkip: Set[String]) extends StepFilter {
  override def skipOver(method: Method): Boolean = skip(method)
  override def skipOut(upperLocation: Location, method: Method): Boolean = skip(method)
  private def skip(method: Method): Boolean =
    classesToSkip.contains(method.declaringType.name) || methodsToSkip.contains(method.toString)
}

private[internal] object RuntimeStepFilter {
  private val javaClassesToSkip = Set("sun.misc.Unsafe")
  private val javaMethodsToSkip = Set(
    "java.lang.invoke.DirectMethodHandle.internalMemberName(java.lang.Object)",
    "java.lang.invoke.DirectMethodHandle.allocateInstance(java.lang.Object)",
    "java.lang.invoke.DirectMethodHandle.constructorMethod(java.lang.Object)"
  )
  private val scalaClassesToSkip = Set(
    "scala.runtime.LazyRef",
    "scala.runtime.LazyBoolean",
    "scala.runtime.LazyChar",
    "scala.runtime.LazyShort",
    "scala.runtime.LazyInt",
    "scala.runtime.LazyLong",
    "scala.runtime.LazyFloat",
    "scala.runtime.LazyDouble",
    "scala.runtime.LazyUnit",
    "scala.runtime.BoxesRunTime"
  )
  private val scala3ClassesToSkip = scalaClassesToSkip ++ Set("scala.runtime.LazyVals$")
  private val scala2ClassesToSkip = scalaClassesToSkip
  private val arrayWrappers = Set(
    "wrapRefArray(java.lang.Object[])",
    "wrapIntArray(int[])",
    "wrapDoubleArray(double[])",
    "wrapLongArray(long[])",
    "wrapFloatArray(float[])",
    "wrapShortArray(short[])",
    "wrapByteArray(byte[])",
    "wrapBooleanArray(boolean[])",
    "wrapUnitArray(scala.runtime.BoxedUnit[])"
  )

  private val scalaMethodsToSkip =
    arrayWrappers.map("scala.runtime.ScalaRunTime$." + _) ++
      arrayWrappers.map("scala.LowPriorityImplicits." + _)

  private val methodsToSkip = javaMethodsToSkip ++ scalaMethodsToSkip

  def apply(scalaVersion: ScalaVersion): RuntimeStepFilter = {
    if (scalaVersion.isScala2)
      new RuntimeStepFilter(scala2ClassesToSkip ++ javaClassesToSkip, methodsToSkip)
    else
      new RuntimeStepFilter(scala3ClassesToSkip ++ scala2ClassesToSkip ++ javaClassesToSkip, methodsToSkip)
  }
}
