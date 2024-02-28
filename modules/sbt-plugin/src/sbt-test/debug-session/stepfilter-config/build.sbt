import ch.epfl.scala.debugadapter.testfmk._
import ch.epfl.scala.debugadapter.StepFiltersConfig

val checkStepFilterOverriding =
  inputKey[Unit]("Check that step filter configuration is overridden with LaunchArguments")

scalaVersion := "3.3.2"
checkStepFilterOverriding := {
  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath
  DebugTest.check(
    uri,
    stepFilters =
      StepFiltersConfig(skipForwardersAndAccessors = true, skipRuntimeClasses = true, skipClassLoading = false)
  )(
    Breakpoint(source, 7),
    StepIn.method("ClassLoader.loadClass(String): Class")
  )
}

val checkNoStepFilterOverridingWhenNoLaunchArgs =
  inputKey[Unit]("Check that step filter configuration is the default one when no LaunchArguments are provided")

scalaVersion := "3.3.2"
checkNoStepFilterOverridingWhenNoLaunchArgs := {
  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath
  DebugTest.check(uri)(Breakpoint(source, 7), StepIn.method("Foo.<init>(x: Int): Unit"))
}
