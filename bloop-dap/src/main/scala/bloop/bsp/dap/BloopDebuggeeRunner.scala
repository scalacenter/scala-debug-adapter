package bloop.bsp.dap

import ch.epfl.scala.bsp
import dap.Logger
import bloop.cli.ExitStatus
import bloop.data.Project
import bloop.engine.State
import bloop.engine.tasks.{RunMode, Tasks}
import bloop.testing.{LoggingEventHandler, TestInternals}
import monix.eval.Task
import scala.concurrent.Future
import monix.execution.Scheduler.Implicits.global
import dap.Cancelable
import java.nio.file.Path
import monix.execution.Scheduler
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import ch.epfl.scala.bsp.ScalaMainClass
import bloop.data.JdkConfig
import xsbti.compile.analysis.SourceInfo
import bloop.data.Platform
import sbt.internal.inc.Analysis
import java.net.InetSocketAddress
import dap.DebugSessionCallbacks


class BloopDebugeeLogger(underlying: bloop.logging.Logger) extends dap.Logger {
  override def debug(msg: => String): Unit = underlying.debug(() => msg)
  override def info(msg: => String): Unit = underlying.info(() => msg)
  override def warn(msg: => String): Unit = underlying.warn(() => msg)
  override def error(msg: => String): Unit = underlying.error(() => msg)
  override def trace(t: => Throwable): Unit = underlying.trace(() => t)
}

abstract class BloopDebuggeeRunner(initialState: State, ioScheduler: Scheduler) extends dap.DebuggeeRunner {
  private lazy val allAnalysis = initialState.results.allAnalysis

  override def logger: dap.Logger = new dap.Logger {
    override def debug(msg: => String): Unit = initialState.logger.debug(() => msg)
    override def info(msg: => String): Unit = initialState.logger.info(() => msg)
    override def warn(msg: => String): Unit = initialState.logger.warn(() => msg)
    override def error(msg: => String): Unit = initialState.logger.error(() => msg)
    override def trace(t: => Throwable): Unit = initialState.logger.trace(() => t)
  }

  override def run(callbacks: DebugSessionCallbacks): Cancelable = {
    val debugSessionLogger = new DebugSessionLogger(callbacks, initialState.logger)
    val task = start(initialState.copy(logger = debugSessionLogger))
      .map { status => 
        new dap.ExitStatus {
          def isOk: Boolean = status.isOk
          def name: String = status.name
        }
      }
      .transform(
        status => callbacks.onFinish(Success(status)),
        t => callbacks.onFinish(Failure(t))
      )
      .doOnCancel(Task.fromFuture(callbacks.onCancel()))
      .runAsync(ioScheduler)
    new Cancelable {
      def cancel(): Unit = task.cancel()
    }
  }
  
  protected def start(state: State): Task[ExitStatus]

  override def classFilesMappedTo(origin: Path, lines: Array[Int], columns: Array[Int]): List[Path] = {
    def isInfoEmpty(info: SourceInfo) = info == sbt.internal.inc.SourceInfos.emptyInfo

    val originFile = origin.toFile
    val foundClassFiles = allAnalysis.collectFirst { analysis =>
      analysis match {
        case analysis if !isInfoEmpty(analysis.infos.get(originFile)) =>
          analysis.relations.products(originFile).iterator.map(_.toPath).toList
      }
    }

    foundClassFiles.toList.flatten
  }
}

private final class MainClassDebugAdapter(
    project: Project,
    mainClass: ScalaMainClass,
    env: JdkConfig,
    initialState: State,
    ioScheduler: Scheduler
) extends BloopDebuggeeRunner(initialState, ioScheduler) {
  def start(state: State): Task[ExitStatus] = {
    val workingDir = state.commonOptions.workingPath
    val runState = Tasks.runJVM(
      state,
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

private final class TestSuiteDebugAdapter(
    projects: Seq[Project],
    filters: List[String],
    initialState: State,
    ioScheduler: Scheduler
) extends BloopDebuggeeRunner(initialState, ioScheduler) {
  def start(state: State): Task[ExitStatus] = {
    val filter = TestInternals.parseFilters(filters)
    val handler = new LoggingEventHandler(state.logger)

    val task = Tasks.test(
      state,
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

private final class AttachRemoteDebugAdapter(initialState: State, ioScheduler: Scheduler
) extends BloopDebuggeeRunner(initialState, ioScheduler) {
  override def start(state: State): Task[ExitStatus] = Task(ExitStatus.Ok)
}

object DebuggeeRunner {
  def forMainClass(
      projects: Seq[Project],
      mainClass: ScalaMainClass,
      state: State,
      ioScheduler: Scheduler
  ): Either[String, dap.DebuggeeRunner] = {
    projects match {
      case Seq() => Left(s"No projects specified for main class: [$mainClass]")
      case Seq(project) =>
        project.platform match {
          case jvm: Platform.Jvm =>
            Right(new MainClassDebugAdapter(project, mainClass, jvm.config, state, ioScheduler))
          case platform =>
            Left(s"Unsupported platform: ${platform.getClass.getSimpleName}")
        }

      case projects => Left(s"Multiple projects specified for main class [$mainClass]: $projects")
    }
  }

  def forTestSuite(
      projects: Seq[Project],
      filters: List[String],
      state: State,
      ioScheduler: Scheduler
  ): Either[String, dap.DebuggeeRunner] = {
    projects match {
      case Seq() => Left(s"No projects specified for the test suites: [${filters.sorted}]")
      case projects => Right(new TestSuiteDebugAdapter(projects, filters, state, ioScheduler))
    }
  }

  def forAttachRemote(state: State, ioScheduler: Scheduler): dap.DebuggeeRunner =
    new AttachRemoteDebugAdapter(state, ioScheduler)

  def classFilesMappedTo(
      origin: Path,
      lines: Array[Int],
      columns: Array[Int],
      allAnalysis: Seq[Analysis]
  ): List[Path] = {
    def isInfoEmpty(info: SourceInfo) = info == sbt.internal.inc.SourceInfos.emptyInfo

    val originFile = origin.toFile
    val foundClassFiles = allAnalysis.collectFirst { analysis =>
      analysis match {
        case analysis if !isInfoEmpty(analysis.infos.get(originFile)) =>
          analysis.relations.products(originFile).iterator.map(_.toPath).toList
      }
    }

    foundClassFiles.toList.flatten
  }
}
