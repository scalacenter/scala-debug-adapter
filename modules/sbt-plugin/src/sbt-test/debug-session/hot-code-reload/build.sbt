import ch.epfl.scala.debugadapter.sbtplugin.DebugAdapterPlugin
import ch.epfl.scala.debugadapter.testfmk._

import com.microsoft.java.debug.core.protocol.Events.OutputEvent.Category
import com.microsoft.java.debug.core.protocol.Types.StackFrame

import java.nio.file.{Files, Path}
import _root_.io.reactivex.subjects.PublishSubject
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

val scalaV = "3.3.0"

val checkBreakpoint =
  inputKey[Unit]("Check the breakpoint of a debug session")
val checkHotCodeReplace =
  taskKey[Unit]("Check the hot code reloading of a debug session")
val source =
  taskKey[Path]("The source file to be tested")

def checkBreakpointTask = Def.inputTask {
  println("Starting debug session")
  val uri = (Compile / startMainClassDebugSession).evaluated
  println(s"Debug session started at $uri")
  implicit val context: TestingContext = TestingContext(source.value, scalaV)

  DebugState.state = DebugTest.init(uri)(Outputed("A"), Breakpoint(6))
}

def checkHotCodeReplaceTask = Def.task {
  val _ = (Compile / compile).value
  DebugTest.runChecks(DebugState.state)(Seq(RedefineClasses(), Outputed("C")))
  DebugTest.endDebugSession(DebugState.state)
}

lazy val hotCodeReloadSingleProject: Project =
  project
    .in(file("."))
    .settings(
      scalaVersion := scalaV,
      checkBreakpoint := checkBreakpointTask.evaluated,
      checkHotCodeReplace := checkHotCodeReplaceTask.value,
      source := (Compile / sources).value.find(_.getName == "A.scala").get.toPath
    )

lazy val hcrMultipleProjects =
  project
    .in(file("./hot-reload-multiple"))
    .settings(
      scalaVersion := scalaV,
      checkBreakpoint := checkBreakpointTask.evaluated,
      checkHotCodeReplace := checkHotCodeReplaceTask.value,
      source := (hotCodeReloadSingleProject / source).value
    )
    .dependsOn(hotCodeReloadSingleProject)
