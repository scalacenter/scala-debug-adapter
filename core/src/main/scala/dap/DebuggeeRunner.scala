package dap

import bloop.cli.ExitStatus
import monix.eval.Task
import java.nio.file.Path
import xsbti.Logger
import ch.epfl.scala.bsp.ScalaMainClass
import xsbti.compile.analysis.SourceInfo
import sbt.internal.inc.Analysis

sealed abstract class DebuggeeRunner {
  def logger: Logger
  def run(logger: DebugSessionLogger): Task[ExitStatus]
  def allAnalysis: Seq[Analysis]

  final def classFilesMappedTo(origin: Path, lines: Array[Int], columns: Array[Int]): List[Path] =
    classFilesMappedTo(origin, lines,  columns, allAnalysis)

  final def classFilesMappedTo(
    origin: Path,
    lines: Array[Int],
    columns: Array[Int],
    allAnalysis: Seq[Analysis]
  ): List[Path] = {
    def isInfoEmpty(info: SourceInfo) = info == sbt.internal.inc.SourceInfos.emptyInfo

    val originFile = origin.toFile
    val foundClassFiles = allAnalysis.collectFirst {
        case analysis if !isInfoEmpty(analysis.infos.get(originFile)) =>
          analysis.relations.products(originFile).iterator.map(_.toPath).toList
    }

    foundClassFiles.toList.flatten
  }
}

//object DebugeeRuner {
//  private def classFilesMappedTo(
//    origin: Path,
//    lines: Array[Int],
//    columns: Array[Int],
//    allAnalysis: Seq[Analysis]
//  ): List[Path] = {
//    def isInfoEmpty(info: SourceInfo) = info == sbt.internal.inc.SourceInfos.emptyInfo
//
//    val originFile = origin.toFile
//    val foundClassFiles = allAnalysis.collectFirst { analysis =>
//      analysis match {
//        case analysis if !isInfoEmpty(analysis.infos.get(originFile)) =>
//          analysis.relations.products(originFile).iterator.map(_.toPath).toList
//      }
//    }
//
//    foundClassFiles.toList.flatten
//  }
//}

abstract class MainClassDebugAdapter(mainClass: ScalaMainClass) extends DebuggeeRunner {
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

abstract class TestSuiteDebugAdapter(filters: List[String]) extends DebuggeeRunner {

  def run(debugLogger: DebugSessionLogger): Task[ExitStatus] = {
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

abstract class AttachRemoteDebugAdapter extends DebuggeeRunner {
  override def run(logger: DebugSessionLogger): Task[ExitStatus] = Task(ExitStatus.Ok)
}

object DebuggeeRunner {
//  def forMainClass(
//      projects: Seq[Project],
//      mainClass: ScalaMainClass,
//      state: State
//  ): Either[String, DebuggeeRunner] = {
//    projects match {
//      case Seq() => Left(s"No projects specified for main class: [$mainClass]")
//      case Seq(project) =>
//        project.platform match {
//          case jvm: Platform.Jvm =>
//            Right(new MainClassDebugAdapter(project, mainClass, jvm.config, state))
//          case platform =>
//            Left(s"Unsupported platform: ${platform.getClass.getSimpleName}")
//        }
//
//      case projects => Left(s"Multiple projects specified for main class [$mainClass]: $projects")
//    }
//  }
//
//  def forTestSuite(
//      projects: Seq[Project],
//      filters: List[String],
//      state: State
//  ): Either[String, DebuggeeRunner] = {
//    projects match {
//      case Seq() => Left(s"No projects specified for the test suites: [${filters.sorted}]")
//      case projects => Right(new TestSuiteDebugAdapter(projects, filters, state))
//    }
//  }
//
//  def forAttachRemote(state: State): DebuggeeRunner =
//    new AttachRemoteDebugAdapter(state)
//
}
