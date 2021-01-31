package dap

import java.io.{File, ObjectInputStream, ObjectOutputStream, Serializable}
import java.net.ServerSocket
import java.nio.file.Path
import sbt.{Fork, ForkConfiguration, ForkMain, ForkOptions, OutputStrategy, React, SuiteResult, TestDefinition, TestFramework, TestReportListener}
import sbt.internal.bsp.BuildTargetIdentifier
import scala.concurrent.ExecutionContext
import xsbti.FileConverter
import sbt.internal.inc.Analysis
import sbt.internal.inc.SourceInfos
import java.lang.{ProcessBuilder => JProcessBuilder}
import sbt.Tests.{ProcessedOptions, overall}
import sbt.internal.util.Terminal
import sbt.protocol.testing.TestResult
import sbt.testing.{AnnotatedFingerprint, Fingerprint, Runner, SubclassFingerprint, TaskDef}
import scala.collection.mutable
import scala.sys.process.{Process, ProcessBuilder}
import scala.util.control.NonFatal

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

private object TestSuiteSbtDebuggeeRunner {
  private def forkFingerprint(f: Fingerprint): Fingerprint with Serializable =
    f match {
      case s: SubclassFingerprint  => new SubclassFingerscan(s)
      case a: AnnotatedFingerprint => new AnnotatedFingerscan(a)
      case _                       => sys.error("Unknown fingerprint type: " + f.getClass)
    }
}

private final class TestSuiteSbtDebuggeeRunner(
    target: BuildTargetIdentifier,
    forkOptions: ForkOptions,
    analyses: Seq[Analysis],
    converter: FileConverter,
    sbtLogger: sbt.util.Logger,
    forkConfiguration: ForkConfiguration,
    /// testRuns
    /// testGroup,
    runners: Map[TestFramework, Runner],
    filters: Array[String],
)(implicit executionContext: ExecutionContext) extends SbtDebuggeeRunner(analyses, converter, sbtLogger) {
  import TestSuiteSbtDebuggeeRunner.forkFingerprint
  override def name: String = s"[Test $filters in ${target.uri}]"
  override def run(callbacks: DebugSessionCallbacks): CancelableFuture[Unit] = {
    val server = new ServerSocket(0)
    val tests: Vector[TestDefinition] = ???
    val setup: Vector[ClassLoader => Unit] = ???
    val cleanup: Vector[ClassLoader => Unit] = ???
    val testListeners: Vector[TestReportListener] = ???

    object Acceptor extends Runnable {
      def run(): Unit = {
        val socket =
          try {
            server.accept()
          } catch {
            case e: java.net.SocketException =>
              sbtLogger.error(
                "Could not accept connection from test agent: " + e.getClass + ": " + e.getMessage
              )
              sbtLogger.trace(e)
              server.close()
              return
          }
        val os = new ObjectOutputStream(socket.getOutputStream)
        // Must flush the header that the constructor writes, otherwise the ObjectInputStream on the other end may block indefinitely
        os.flush()
        val is = new ObjectInputStream(socket.getInputStream)

        try {
          os.writeObject(forkConfiguration)

          val taskdefs = tests.map { t =>
            new TaskDef(
              t.name,
              forkFingerprint(t.fingerprint),
              t.explicitlySpecified,
              t.selectors
            )
          }
          os.writeObject(taskdefs.toArray)

          os.writeInt(runners.size)
          for ((testFramework, mainRunner) <- runners) {
            os.writeObject(testFramework.implClassNames.toArray)
            os.writeObject(mainRunner.args)
            os.writeObject(mainRunner.remoteArgs)
          }
          os.flush()

          //new React(is, os, log, opts.testListeners, resultsAcc).react()
        } catch {
          case NonFatal(e) =>
            def throwableToString(t: Throwable) = {
              import java.io._; val sw = new StringWriter; t.printStackTrace(new PrintWriter(sw));
              sw.toString
            }
            //resultsAcc("Forked test harness failed: " + throwableToString(e)) = SuiteResult.Error
        } finally {
          is.close(); os.close(); socket.close()
        }
      }
    }

    val acceptorThread = new Thread(Acceptor)
    acceptorThread.start()

    val mainClass = classOf[ForkMain].getCanonicalName
    val args = Seq(server.getLocalPort.toString)

    sbtLogger.info(s"running test class debuggee: $filters")
    val process = DebuggeeProcess.start(forkOptions, Some(mainClass), args, callbacks)
    process.future.onComplete(_ => server.close())

    process
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