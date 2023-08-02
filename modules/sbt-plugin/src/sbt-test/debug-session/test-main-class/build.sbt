import ch.epfl.scala.debugadapter.testfmk._

val checkDebugSession = inputKey[Unit]("Check the main class debug session")

scalaVersion := "2.12.14"
checkDebugSession := {
  val uri = (Test / startMainClassDebugSession).evaluated
  val source = (Test / sources).value.head.toPath
  DebugTest.check(uri)(Breakpoint(source, 5))
}
