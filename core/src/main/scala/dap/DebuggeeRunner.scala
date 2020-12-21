package dap

import bloop.cli.ExitStatus
import monix.eval.Task
import java.nio.file.Path
import xsbti.Logger
import ch.epfl.scala.bsp.ScalaMainClass
import xsbti.compile.analysis.SourceInfo
import sbt.internal.inc.Analysis
import scala.concurrent.Future

sealed abstract class DebuggeeRunner {
  def logger: Logger
  def run(logger: DebugSessionLogger): Future[ExitStatus]

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

abstract class MainClassDebugAdapter(mainClass: ScalaMainClass) extends DebuggeeRunner {

}

abstract class TestSuiteDebugAdapter(filters: List[String]) extends DebuggeeRunner {

}

abstract class AttachRemoteDebugAdapter extends DebuggeeRunner {
  override def run(logger: DebugSessionLogger): Future[ExitStatus] = Future.successful(ExitStatus.Ok)
}
