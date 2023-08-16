import ch.epfl.scala.debugadapter.testfmk._

import java.nio.file.Path
import scala.concurrent.ExecutionContext.Implicits.global

val scalaV = "3.3.0"

val checkBreakpoint = inputKey[Unit]("Check the breakpoint of a debug session")
val checkHotCodeReplace = taskKey[Unit]("Check the hot code reloading of a debug session")
val sourceToDebug = taskKey[Path]("The source file to be tested")

def checkBreakpointTask = Def.inputTask {
  val uri = (Compile / startMainClassDebugSession).evaluated
  implicit val context: TestingContext = TestingContext(sourceToDebug.value, scalaV)
  DebugState.state = DebugTest.init(uri)(Outputed("A"), Breakpoint(6))
}

def checkHotCodeReplaceTask = Def.task {
  val _ = (Compile / compile).value
  DebugTest.runChecks(DebugState.state)(Seq(RedefineClasses(), Outputed("C")))
}

lazy val a: Project =
  project
    .in(file("."))
    .settings(
      scalaVersion := scalaV,
      checkBreakpoint := checkBreakpointTask.evaluated,
      checkHotCodeReplace := checkHotCodeReplaceTask.value,
      sourceToDebug := (Compile / sources).value.find(_.getName == "A.scala").get.toPath
    )

lazy val b =
  project
    .in(file("b"))
    .settings(
      scalaVersion := scalaV,
      checkBreakpoint := checkBreakpointTask.evaluated,
      checkHotCodeReplace := checkHotCodeReplaceTask.value,
      sourceToDebug := (a / sourceToDebug).value
    )
    .dependsOn(a)
