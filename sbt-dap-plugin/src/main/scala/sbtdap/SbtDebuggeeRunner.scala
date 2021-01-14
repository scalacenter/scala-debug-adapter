//package sbtdap
//
//import dap.{AttachRemoteDebugAdapter, DebuggeeRunner, MainClassDebugAdapter, TestSuiteDebugAdapter}
//import java.nio.file.Path
//import sbt.{Keys, Project, State}
//import sbt.internal.bsp.ScalaMainClass
//import sbt.internal.inc.Analysis
//import xsbti.compile.analysis.SourceInfo
//
//private trait SbtTestSuiteDebugAdapter {
//  def projects: Seq[Project]
//  def state: State
//  override def allAnalysis: Seq[Analysis] = ???
//}
//
//private final class SbtMainClassDebugAdapter(projects: Seq[Project], state: State, mainClass: ScalaMainClass) extends
//  MainClassDebugAdapter(mainClass: ScalaMainClass) with SbtTestSuiteDebugAdapter {
//
//  def run(debugLogger: DebugSessionLogger): Task[ExitStatus] = {
//    //    val workingDir = state.commonOptions.workingPath
//    //    val runState = Tasks.runJVM(
//    //      state.copy(logger = debugLogger),
//    //      project,
//    //      env,
//    //      workingDir,
//    //      mainClass.`class`,
//    //      (mainClass.arguments ++ mainClass.jvmOptions).toArray,
//    //      skipJargs = false,
//    //      mainClass.environmentVariables,
//    //      RunMode.Debug
//    //    )
//    //
//    //    runState.map(_.status)
//    ???
//  }
//}
//
//private final class SbtTestSuiteDebugAdapter(projects: Seq[Project], state: State, filters: List[String]) extends
//  TestSuiteDebugAdapter(filters) with SbtTestSuiteDebugAdapter {
//
//  override def run(debugLogger: DebugSessionLogger): Task[ExitStatus] = {
//    //    val debugState = state.copy(logger = debugLogger)
//    //
//    //    val filter = TestInternals.parseFilters(filters)
//    //    val handler = new LoggingEventHandler(debugState.logger)
//    //
//    //    val task = Tasks.test(
//    //      debugState,
//    //      projects.toList,
//    //      Nil,
//    //      filter,
//    //      handler,
//    //      runInParallel = false,
//    //      mode = RunMode.Debug
//    //    )
//    //
//    //    task.map(_.status)
//    ???
//  }
//}
//
//private final class SbtAttachRemoteDebugAdapter extends AttachRemoteDebugAdapter {
//
//}
