import scala.concurrent.ExecutionContext
import ch.epfl.scala.debugadapter.testfmk._

val checkDebugSession = inputKey[Unit]("Check the main class debug session")

scalaVersion := "3.0.0"
checkDebugSession := {
  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath

  DebugTest.check(uri)(Breakpoint(source, 5))
}
