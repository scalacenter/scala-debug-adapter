package ch.epfl.scala.debugadapter.sbtplugin.internal

import ch.epfl.scala.debugadapter.CancelableFuture
import ch.epfl.scala.debugadapter.DebuggeeListener
import ch.epfl.scala.debugadapter.Logger
import sbt.ForkOptions
import sbt.io.syntax._

import java.io.File
import java.nio.file.Path
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.sys.process.Process
import scala.util.Failure
import scala.util.Success

private class DebuggeeProcess(process: Process) extends CancelableFuture[Unit] {
  private val exited = Promise[Unit]()

  DebuggeeProcess.fork { () =>
    val exitCode = process.exitValue()
    if (exitCode != 0)
      exited.failure(
        new Exception(s"""Nonzero exit code returned: $exitCode""".stripMargin)
      )
    else exited.success(())
  }

  override val future: Future[Unit] = exited.future

  override def cancel(): Unit = process.destroy()
}

private object DebuggeeProcess {
  private final val debugInterface: String =
    "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,quiet=n"

  def start(
      forkOptions: ForkOptions,
      classpath: Seq[Path],
      mainClass: String,
      arguments: Seq[String],
      listener: DebuggeeListener,
      logger: Logger
  )(implicit ec: ExecutionContext): DebuggeeProcess = {
    val javaHome = forkOptions.javaHome.getOrElse(new File(System.getProperty("java.home")))
    val javaBin = (javaHome / "bin" / "java").getAbsolutePath

    val classpathOption = classpath.mkString(File.pathSeparator)
    val envVars = forkOptions.envVars + ("CLASSPATH" -> classpathOption)

    // remove agentlib option specified by the user
    val jvmOptions = forkOptions.runJVMOptions.filter(opt => !opt.contains("-agentlib"))
    val command = Seq(javaBin, debugInterface) ++ jvmOptions ++ Some(mainClass) ++ arguments

    val builder = Process(command, forkOptions.workingDirectory, envVars.toSeq: _*)
    val processLogger = new DebuggeeProcessLogger(listener)

    logger.info("Starting debuggee process")
    logger.debug(command.mkString(" "))
    logger.debug(s"working directory: ${forkOptions.workingDirectory.getOrElse("null")}")
    logger.debug(s"env vars:\n${envVars.map { case (key, value) => s"  $key=$value " }.mkString("\n")}")

    val process = new DebuggeeProcess(builder.run(processLogger))
    process.future.onComplete {
      case Failure(cause) =>
        logger.warn(s"Debuggee process failed with ${cause.getMessage}")
      case Success(()) =>
        logger.info(s"Debuggee process terminated successfully.")
    }
    process
  }

  private def fork(f: () => Unit): Unit = {
    val thread = new Thread {
      override def run(): Unit = f()
    }
    thread.start()
  }
}
