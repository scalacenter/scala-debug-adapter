import ch.epfl.scala.debugadapter.testfmk._

val checkDebugSession = inputKey[Unit]("Check the test suite debug session")

libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.10" % Test
testFrameworks += new TestFramework("utest.runner.Framework")
scalaVersion := "2.12.14"
checkDebugSession := {
  val uri = (Test / startTestSuitesDebugSession).evaluated
  val mainSource = (Compile / sources).value.head.toPath
  val suiteSource = (Test / sources).value.head.toPath

  DebugTest.check(uri)(Breakpoint(suiteSource, 8), Breakpoint(mainSource, 5), Breakpoint(suiteSource, 10))
}
