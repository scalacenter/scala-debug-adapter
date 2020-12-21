package sbtdap

import dap.{AttachRemoteDebugAdapter, DebuggeeRunner, MainClassDebugAdapter, TestSuiteDebugAdapter}
import java.nio.file.Path
import sbt.{Keys, Project, State}
import sbt.internal.bsp.ScalaMainClass
import sbt.internal.inc.Analysis
import xsbti.compile.analysis.SourceInfo

object SbtDebuggeeRunner {
  def forMainClass(
      projects: Seq[Project],
      mainClass: ScalaMainClass,
      state: State
  ): Either[String, DebuggeeRunner] = {
    projects match {
      case Seq() => Left(s"No projects specified for main class: [$mainClass]")
      case Seq(project) => Right(new MainClassDebugAdapter(project, mainClass, state))
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
      case projects => Right(new TestSuiteDebugAdapter(projects, filters, state))
    }
  }

  def forAttachRemote(state: State): DebuggeeRunner =
    new AttachRemoteDebugAdapter(state)

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
