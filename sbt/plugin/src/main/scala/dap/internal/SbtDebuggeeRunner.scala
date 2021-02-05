package dap.internal

import sbt.Def.Classpath
import sbt.Tests.{Cleanup, Setup}
import sbt.internal.bsp.BuildTargetIdentifier
import sbt.internal.inc.{Analysis, SourceInfos}
import sbt.io.IO
import sbt.testing._
import sbt.{ForkOptions, TestDefinition, TestFramework}
import xsbti.FileConverter

import java.io.{ObjectOutputStream, Serializable}
import java.net.{ServerSocket, Socket}
import java.nio.file.Path
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import sbt.Keys
import scala.concurrent.Future
import dap._

private[dap] abstract class SbtDebuggeeRunner(classpath: Classpath, converter: FileConverter) extends DebuggeeRunner {
  private val analyses = classpath
    .flatMap(_.metadata.get(Keys.analysis))
    .map { case a: Analysis => a }

  override def classFilesMappedTo(origin: Path, lines: Array[Int], columns: Array[Int]): List[Path] = {
    val originRef = converter.toVirtualFile(origin)
    analyses.collectFirst {
      case analysis if analysis.infos.get(originRef) != SourceInfos.emptyInfo =>
        analysis.relations.products(originRef).iterator.map(converter.toPath).toList
    }.getOrElse(List.empty)
  }
}

private[dap] final class MainClassRunner(
  target: BuildTargetIdentifier,
  forkOptions: ForkOptions,
  classpath: Classpath,
  mainClass: String,
  args: Seq[String],
  converter: FileConverter
)(implicit ec: ExecutionContext) extends SbtDebuggeeRunner(classpath, converter) {
  override def name: String = s"${getClass.getSimpleName}(${target.uri}, $mainClass)"

  override def run(logger: DebuggeeLogger): CancelableFuture[Unit] = {
    DebuggeeProcess.start(forkOptions, classpath.map(_.data), mainClass, args, logger)
  }
}

private[dap] final class TestSuitesRunner(
    target: BuildTargetIdentifier,
    forkOptions: ForkOptions,
    classpath: Classpath,
    setups: Seq[Setup],
    cleanups: Seq[Cleanup],
    parallel: Boolean, 
    runners: Map[TestFramework, Runner],
    tests: Seq[TestDefinition],
    converter: FileConverter
)(implicit executionContext: ExecutionContext) extends SbtDebuggeeRunner(classpath, converter) {
  override def name: String = s"${getClass.getSimpleName}(${target.uri}, [${tests.mkString(", ")}])"
  override def run(logger: DebuggeeLogger): CancelableFuture[Unit] = {
    object Acceptor extends Thread {
      val server = new ServerSocket(0)
      var socket: Socket = _
      var os: ObjectOutputStream = _

      override def run(): Unit = {
        try { 
          socket = server.accept()
          os = new ObjectOutputStream(socket.getOutputStream)
          
          // Must flush the header that the constructor writes
          // otherwise the ObjectInputStream on the other end may block indefinitely
          os.flush()

          val config = new ForkConfiguration(true, parallel)
          os.writeObject(config)

          val taskdefs = tests.map { t =>
            val forkFingerprint = t.fingerprint match {
              case s: SubclassFingerprint  => new SubclassFingerscan(s)
              case a: AnnotatedFingerprint => new AnnotatedFingerscan(a)
              case f                       => sys.error("Unknown fingerprint type: " + f.getClass)
            }
            new TaskDef(
              t.name,
              forkFingerprint,
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
        } catch {
          case NonFatal(cause) => close()
        }
      }

      def close(): Unit = {
        if (os != null) os.close()
        if (socket != null) socket.close()
        server.close()
      }
    }

    Acceptor.start()

    val mainClass = classOf[ForkMain].getCanonicalName
    val args = Seq(Acceptor.server.getLocalPort.toString)

    val fullClasspath = classpath.map(_.data) ++ Seq(
      IO.classLocationPath[ForkMain].toFile, // debug-test-agent
      IO.classLocationPath[Framework].toFile // test-interface
    )

    // can't provide the loader for test classes, which is in another jvm
    val dummyLoader = this.getClass().getClassLoader()

    setups.foreach(_.setup(dummyLoader))
    val process = DebuggeeProcess.start(forkOptions, fullClasspath, mainClass, args, logger)
    process.future.onComplete { _=>
      Acceptor.close()
      cleanups.foreach(_.cleanup(dummyLoader))
    }

    process
  }
}

private[dap] final case class AttachRemoteRunner(
  target: BuildTargetIdentifier,
  classpath: Classpath,
  converter: FileConverter
) extends SbtDebuggeeRunner(classpath, converter) {
  override def name: String = s"${getClass.getSimpleName}(${target.uri})"
  override def run(logger: DebuggeeLogger): CancelableFuture[Unit] = new CancelableFuture[Unit] {
    override def future: Future[Unit] = Future.successful(())
    override def cancel(): Unit = ()
  }
}