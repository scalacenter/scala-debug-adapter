package bloop.bsp.dap

import ch.epfl.scala.bsp
import _root_.dap.{AttachRemoteDebugAdapter => DapAttachRemoteDebugAdapter, MainClassDebugAdapter => DapMainClassDebugAdapter, TestSuiteDebugAdapter => DapTestSuiteDebugAdapter}
import _root_.dap.{DebugSessionLogger, DebuggeeRunner}
import bloop.cli.ExitStatus
import bloop.engine.tasks.{RunMode, Tasks}
import bloop.testing.{LoggingEventHandler, TestInternals}
import monix.eval.Task
import scala.concurrent.Future
import monix.execution.Scheduler.Implicits.global

object BloopDebuggeeRunnerFactory extends _root_.dap.DebuggeeRunnerFactory[bloop.data.Project, bloop.engine.State] {
  type Project = bloop.data.Project
  type State = bloop.engine.State

  trait BloopDebuggeeRunner {
    final def run(logger: DebugSessionLogger): Future[ExitStatus] = runImpl(logger).runAsync
    protected def runImpl(logger: DebugSessionLogger): Task[ExitStatus]
  }

  protected def makeMainClassDebugAdapter(
    project: Project,
    state: State,
    mainClass: bsp.ScalaMainClass
  ): DapMainClassDebugAdapter = new MainClassDebugAdapter(project, state, mainClass)

  protected def makeTestSuiteDebugAdapter(
    projects: Seq[Project],
    state: State,
    filters: List[String],
  ): DapTestSuiteDebugAdapter = new TestSuiteDebugAdapter(projects, state, filters)

  protected def makeAttachRemoteDebugAdapter(state: State): DapAttachRemoteDebugAdapter =
    new AttachRemoteDebugAdapter(state)


  final class MainClassDebugAdapter(
    project: Project,
    state: State,
    mainClass: bsp.ScalaMainClass
  ) extends DapMainClassDebugAdapter(mainClass: bsp.ScalaMainClass) with BloopDebuggeeRunner {

    override def runImpl(debugLogger: DebugSessionLogger): Task[ExitStatus] = {
      val workingDir = state.commonOptions.workingPath
      val runState = Tasks.runJVM(
        state.copy(logger = debugLogger),
        project,
        env,
        workingDir,
        mainClass.`class`,
        (mainClass.arguments ++ mainClass.jvmOptions).toArray,
        skipJargs = false,
        mainClass.environmentVariables,
        RunMode.Debug
      )

      runState.map(_.status)
    }
  }

  final class TestSuiteDebugAdapter(
    projects: Seq[Project],
    state: State,
    filters: List[String]
  ) extends DapTestSuiteDebugAdapter(filters) with BloopDebuggeeRunner {

    override def runImpl(debugLogger: DebugSessionLogger): Task[ExitStatus] = {
      val debugState = state.copy(logger = debugLogger)

      val filter = TestInternals.parseFilters(filters)
      val handler = new LoggingEventHandler(debugState.logger)

      val task = Tasks.test(
        debugState,
        projects.toList,
        Nil,
        filter,
        handler,
        runInParallel = false,
        mode = RunMode.Debug
      )

      task.map(_.status)
    }
  }

  final class AttachRemoteDebugAdapter(state: State) extends DapAttachRemoteDebugAdapter {

  }
}


