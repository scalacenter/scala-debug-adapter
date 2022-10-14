import ch.epfl.scala.debugadapter.sbtplugin.DebugAdapterPlugin
import ch.epfl.scala.debugadapter.testfmk._

val checkDebugSession =
  inputKey[Unit]("Check the integration test suite debug session")

val root = project
  .in(file("."))
  .configs(IntegrationTest)
  .settings(
    scalaVersion := "2.12.14",
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.10" % IntegrationTest,
    testFrameworks += new TestFramework("utest.runner.Framework"),
    Defaults.itSettings,
    inConfig(IntegrationTest)(DebugAdapterPlugin.testSettings),
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
