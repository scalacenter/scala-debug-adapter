package sbtdap

import dap.{AttachRemoteDebugAdapter, DebuggeeRunner, MainClassDebugAdapter, TestSuiteDebugAdapter}
import java.nio.file.Path
import sbt.{Keys, Project, State}
import sbt.internal.bsp.ScalaMainClass
import sbt.internal.inc.Analysis
import xsbti.compile.analysis.SourceInfo

private trait SbtTestSuiteDebugAdapter {
  def projects: Seq[Project]
  def state: State
  override def allAnalysis: Seq[Analysis] = ???
}

private final class SbtMainClassDebugAdapter(projects: Seq[Project], state: State, mainClass: ScalaMainClass) extends
  MainClassDebugAdapter(mainClass: ScalaMainClass) with SbtTestSuiteDebugAdapter {

  def run(debugLogger: DebugSessionLogger): Task[ExitStatus] = {
    //    val workingDir = state.commonOptions.workingPath
    //    val runState = Tasks.runJVM(
    //      state.copy(logger = debugLogger),
    //      project,
    //      env,
    //      workingDir,
    //      mainClass.`class`,
    //      (mainClass.arguments ++ mainClass.jvmOptions).toArray,
    //      skipJargs = false,
    //      mainClass.environmentVariables,
    //      RunMode.Debug
    //    )
    //
    //    runState.map(_.status)
    ???
  }
}

private final class SbtTestSuiteDebugAdapter(projects: Seq[Project], state: State, filters: List[String]) extends
  TestSuiteDebugAdapter(filters) with SbtTestSuiteDebugAdapter {

  override def run(debugLogger: DebugSessionLogger): Task[ExitStatus] = {
    //    val debugState = state.copy(logger = debugLogger)
    //
    //    val filter = TestInternals.parseFilters(filters)
    //    val handler = new LoggingEventHandler(debugState.logger)
    //
    //    val task = Tasks.test(
    //      debugState,
    //      projects.toList,
    //      Nil,
    //      filter,
    //      handler,
    //      runInParallel = false,
    //      mode = RunMode.Debug
    //    )
    //
    //    task.map(_.status)
    ???
  }
}

private final class SbtAttachRemoteDebugAdapter extends AttachRemoteDebugAdapter {

}

object SbtDebuggeeRunner {
  def forMainClass(
      projects: Seq[Project],
      state: State,
      mainClass: ScalaMainClass,
  ): Either[String, DebuggeeRunner] = {
    projects match {
      case Seq() => Left(s"No projects specified for main class: [$mainClass]")
      case Seq(project) => Right(new SbtMainClassDebugAdapter(project, state, mainClass))
      case projects => Left(s"Multiple projects specified for main class [$mainClass]: $projects")
    }
  }

  def forTestSuite(
      projects: Seq[Project],
      filters: List[String],
      state: State
  ): Either[String, DebuggeeRunner] = {
    projects match {
      case Seq() => Left(s"No projects specified for the test suites: [${filters.sorted}]")
      case projects => Right(new SbtTestSuiteDebugAdapter(projects, state, filters))
    }
  }

  def forAttachRemote(state: State): DebuggeeRunner =
    new SbtAttachRemoteDebugAdapter(state)
}
