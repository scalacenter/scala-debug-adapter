package dap

import java.nio.file.Path
import sbt.{Project, State}
import sbt.internal.bsp

import scala.util.{Failure, Success}
import sjsonnew.JsonFormat
import sjsonnew.support.scalajson.unsafe.Converter
import sjsonnew.shaded.scalajson.ast.unsafe.JValue

import java.net.URI
import scala.concurrent.Future
import java.io.File
import sbt.ForkOptions
import sbt.internal.bsp.BuildTargetIdentifier
import sbt.Run
import scala.concurrent.ExecutionContext
import sbt.Fork
import sbt.OutputStrategy
import xsbti.compile.CompileAnalysis
import xsbti.FileConverter
import xsbti.compile.CompileResult
import sbt.internal.inc.Analysis
import sbt.internal.inc.SourceInfos


private abstract class SbtDebuggeeRunner(analyses: Array[Analysis], converter: FileConverter, sbtLog: sbt.Logger) extends DebuggeeRunner {
  final override def logger = ???
  final override def classFilesMappedTo(origin: Path, lines: Array[Int], columns: Array[Int]): List[Path] = {
    val originRef = converter.toVirtualFile(origin)
    analyses.collectFirst {
      case analysis if analysis.infos.get(originRef) != SourceInfos.emptyInfo =>
        analysis.relations.products(originRef).iterator.map(converter.toPath).toList
    }.getOrElse(List.empty)
  }
}

private final class MainClassSbtDebuggeeRunner(
  target: BuildTargetIdentifier,
  forkOptions: ForkOptions,
  mainClass: String,
  args: Seq[String],
  analyses: Array[Analysis],
  converter: FileConverter,
  sbtLogger: sbt.util.Logger
)(implicit ec: ExecutionContext) extends SbtDebuggeeRunner(analyses, converter, sbtLogger) {
  override def name: String = s"[Main class $mainClass in ${target.uri}]"

  override def run(callbacks: DebugSessionCallbacks): CancelableFuture[Unit] = { 
    sbtLogger.info(s"running main class debuggee: $mainClass")
    DebuggeeProcess.start(forkOptions, Some(mainClass), args, callbacks)
  }
}

private final class TestSuiteSbtDebuggeeRunner(
  analyses: Array[Analysis],
  converter: FileConverter,
  sbtLogger: sbt.Logger
) extends SbtDebuggeeRunner(analyses, converter, sbtLogger) {

  override def name: String = ???
  override def run(callbacks: DebugSessionCallbacks): CancelableFuture[Unit] = {
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

private final case class AttachRemoteSbtDebuggeeRunner(
  analyses: Array[Analysis],
  converter: FileConverter,
  sbtLogger: sbt.Logger
) extends SbtDebuggeeRunner(analyses, converter, sbtLogger) {
  override def name: String = ???
  override def run(callbacks: DebugSessionCallbacks): CancelableFuture[Unit] = ???
}