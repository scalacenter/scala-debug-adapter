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
def source = Def.task((Compile / sources).value.find(_.getName == "A.scala").get.toPath)

def checkBreakpointTask = Def.inputTask {
  val uri = (Compile / startMainClassDebugSession).evaluated
  implicit val context: TestingContext = TestingContext(source.value, scalaV)

  def runChecks = DebugTest.init(uri) _
  DebugState.state = runChecks(Seq(Outputed("A"), Breakpoint(6)))
}

def checkHotCodeReplaceTask = Def.task {
  DebugTest.runChecks(DebugState.state)(Seq(RedefineClasses(), Outputed("C")))
  DebugTest.endDebugSession(DebugState.state)
}

lazy val hotCodeReload =
  project
    .in(file("."))
    .enablePlugins(SbtJdiTools)
    .settings(
      scalaVersion := scalaV,
      checkBreakpoint := checkBreakpointTask.evaluated,
      checkHotCodeReplace := checkHotCodeReplaceTask.value
    )
