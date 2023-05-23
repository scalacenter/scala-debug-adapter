import ch.epfl.scala.debugadapter.testfmk._

val checkDebugSession = inputKey[Unit]("Check the main class debug session")

// there is no expression compiler for Scala 2.11
scalaVersion := "2.11.12"

// check that one can start a debug session even if the expression compiler
// cannot be resolved
checkDebugSession := {
  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath

  implicit val ctx = TestingContext(source, scalaVersion.value)
  DebugTest.check(uri)(Breakpoint(4), Evaluation.failed("s\"${1 + 1}\"", "Missing scala-expression-compiler"))
}
