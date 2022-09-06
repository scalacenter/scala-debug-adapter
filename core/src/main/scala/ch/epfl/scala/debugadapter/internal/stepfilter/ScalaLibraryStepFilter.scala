package ch.epfl.scala.debugadapter.internal.stepfilter

import com.sun.jdi.Method
import ch.epfl.scala.debugadapter.DebuggeeRunner

private class ScalaLibraryStepFilter(classesToSkip: Set[String])
    extends StepFilter {
  override def skip(method: Method): Boolean =
    classesToSkip.contains(method.declaringType.name)
}

private[internal] object ScalaLibraryStepFilter {
  private val scala3ClassesToSkip = Set("scala.runtime.LazyVals$")
  private val scala2ClassesToSkip = Set.empty[String]

  def apply(runner: DebuggeeRunner): StepFilter = {
    if (runner.scalaVersion.startsWith("2"))
      new ScalaLibraryStepFilter(scala2ClassesToSkip)
    else new ScalaLibraryStepFilter(scala3ClassesToSkip ++ scala2ClassesToSkip)
  }
}
