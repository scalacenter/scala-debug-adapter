package ch.epfl.scala.debugadapter.sbtplugin.internal

import ch.epfl.scala.debugadapter._
import sbt.Def.Classpath
import sbt.Tests.{Cleanup, Setup}
import sbt.internal.bsp.BuildTargetIdentifier
import sbt.io.IO
import sbt.testing._
import sbt.{ForkOptions, TestDefinition, TestFramework}

import java.io.ObjectOutputStream
import java.net.{ServerSocket, Socket}
import java.nio.file.Path
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

private[debugadapter] final class MainClassRunner(
  target: BuildTargetIdentifier,
  forkOptions: ForkOptions,
  val classPathEntries: Seq[ClassPathEntry],
  mainClass: String,
  args: Seq[String]
)(implicit ec: ExecutionContext) extends DebuggeeRunner {
  override def name: String = s"${getClass.getSimpleName}(${target.uri}, $mainClass)"

  override def run(listener: DebuggeeListener): CancelableFuture[Unit] = {
    DebuggeeProcess.start(forkOptions, classPath, mainClass, args, listener)
  }
}

private[debugadapter] final class TestSuitesRunner(
    target: BuildTargetIdentifier,
    forkOptions: ForkOptions,
    val classPathEntries: Seq[ClassPathEntry],
    setups: Seq[Setup],
    cleanups: Seq[Cleanup],
    parallel: Boolean, 
    runners: Map[TestFramework, Runner],
    tests: Seq[TestDefinition]
)(implicit executionContext: ExecutionContext) extends DebuggeeRunner {
  override def name: String = s"${getClass.getSimpleName}(${target.uri}, [${tests.mkString(", ")}])"
  override def run(listener: DebuggeeListener): CancelableFuture[Unit] = {
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
          case NonFatal(_) => close()
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

    val fullClasspath = classPath ++ Seq(
      IO.classLocationPath[ForkMain], // debug-test-agent
      IO.classLocationPath[Framework] // test-interface
    )

    // can't provide the loader for test classes, which is in another jvm
    val dummyLoader = getClass.getClassLoader

    setups.foreach(_.setup(dummyLoader))
    val process = DebuggeeProcess.start(forkOptions, fullClasspath, mainClass, args, listener)
    process.future.onComplete { _=>
      Acceptor.close()
      cleanups.foreach(_.cleanup(dummyLoader))
    }

    process
  }
}

private[debugadapter] final class AttachRemoteRunner(
  target: BuildTargetIdentifier,
  val classPathEntries: Seq[ClassPathEntry]
) extends DebuggeeRunner {
  override def name: String = s"${getClass.getSimpleName}(${target.uri})"
  override def run(listener: DebuggeeListener): CancelableFuture[Unit] = new CancelableFuture[Unit] {
    override def future: Future[Unit] = Future.successful(())
    override def cancel(): Unit = ()
  }
}