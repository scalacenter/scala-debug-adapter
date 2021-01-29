package dap

import java.nio.file.Path
import sbt.ForkOptions
import sbt.internal.bsp.BuildTargetIdentifier
import scala.concurrent.ExecutionContext
import xsbti.FileConverter
import sbt.internal.inc.Analysis
import sbt.internal.inc.SourceInfos

private abstract class SbtDebuggeeRunner(analyses: Seq[Analysis], converter: FileConverter, sbtLog: sbt.Logger) extends DebuggeeRunner {
  import SbtLoggerAdapter._
  final override def logger: Logger = sbtLog

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
  analyses: Seq[Analysis],
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
    target: BuildTargetIdentifier,
    forkOptions: ForkOptions,
    analyses: Seq[Analysis],
    converter: FileConverter,
    sbtLogger: sbt.util.Logger
    /// forkConfiguration,
    /// testRuns
    /// testGroup,
    // testFramework
) extends SbtDebuggeeRunner(analyses, converter, sbtLogger) {

  override def name: String = ???
  override def run(callbacks: DebugSessionCallbacks): CancelableFuture[Unit] = {
    // create the server
    // start the process, main is ForkMain and args is the port of the socket
    // connect
    // send the fork config then the frameworks and runs

    // similar code can be found in
    // https://github.com/sbt/sbt/blob/c4c88b75e448851720ccf8cfc54191a4d28f48a7/main-actions/src/main/scala/sbt/ForkTests.scala#L61-L178
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