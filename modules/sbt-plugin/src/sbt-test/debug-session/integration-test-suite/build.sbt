import ch.epfl.scala.debugadapter.sbtplugin.DebugAdapterPlugin
import ch.epfl.scala.debugadapter.testfmk._

val checkDebugSession =
  inputKey[Unit]("Check the integration test suite debug session")

// sbt 2 removed the built-in IntegrationTest config and Defaults.itSettings;
// define an `it` config that works on both sbt 1 and sbt 2.
val IntegrationTest = config("it").extend(Test)

val root = project
  .in(file("."))
  .configs(IntegrationTest)
  .settings(
    scalaVersion := "2.12.14",
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.10" % IntegrationTest,
    testFrameworks += new TestFramework("utest.runner.Framework"),
    inConfig(IntegrationTest)(Defaults.testSettings ++ DebugAdapterPlugin.testSettings),
    checkDebugSession := checkDebugSessionTask.evaluated
  )

def checkDebugSessionTask = Def.inputTask {
  val uri = (IntegrationTest / startTestSuitesDebugSession).evaluated
  val mainSource = (Compile / sources).value.head.toPath
  val suiteSource = (IntegrationTest / sources).value.head.toPath

  DebugTest.check(uri)(
    Breakpoint(suiteSource, 8),
    Breakpoint(mainSource, 5),
    Breakpoint(suiteSource, 10)
  )
}
