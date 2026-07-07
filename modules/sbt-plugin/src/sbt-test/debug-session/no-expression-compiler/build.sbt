import ch.epfl.scala.debugadapter.testfmk._

val checkDebugSession = inputKey[Unit]("Check the main class debug session")

// there is no expression compiler for Scala 2.11
val root = project
  .in(file("."))
  .settings(
    scalaVersion := "2.11.12",
    // check that one can start a debug session even if the expression compiler
    // cannot be resolved
    checkDebugSession := checkDebugSessionTask.evaluated
  )

def checkDebugSessionTask = Def.inputTask {
  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath

  implicit val ctx: TestingContext = TestingContext(source, "2.11.12")
  DebugTest.check(uri)(Breakpoint(4), Evaluation.failed("s\"${1 + 1}\"", "Cannot evaluate"))
}
