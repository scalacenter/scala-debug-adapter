package ch.epfl.scala.debugadapter.sbtplugin.internal

import ch.epfl.scala.debugadapter.{CancelableFuture, DebuggeeListener}
import sbt.ForkOptions
import sbt.io.syntax._

import scala.concurrent.{Future, Promise}
import scala.sys.process.Process
import java.nio.file.Path
import java.io.File

private class DebuggeeProcess(process: Process) extends CancelableFuture[Unit] {
  private val exited = Promise[Unit]()

  DebuggeeProcess.fork { () =>
    val exitCode = process.exitValue()
    if (exitCode != 0)
      exited.failure(new Exception(s"""Nonzero exit code returned: $exitCode""".stripMargin))
    else exited.success(())
  }

  override def future: Future[Unit] = exited.future
  override def cancel(): Unit = process.destroy()
}

private object DebuggeeProcess {
  private final val debugInterface: String = "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,quiet=n"

  def start(
    forkOptions: ForkOptions,
    classpath: Seq[Path],
    mainClass: String,
    arguments: Seq[String],
    listener: DebuggeeListener
  ): DebuggeeProcess = {
    val javaHome = forkOptions.javaHome.getOrElse(new File(System.getProperty("java.home")))
    val javaBin = (javaHome / "bin" / "java").getAbsolutePath
    
    val classpathOption = classpath.mkString(File.pathSeparator)
    val envVars = forkOptions.envVars + ("CLASSPATH" -> classpathOption)

    val command = Seq(javaBin, debugInterface) ++
      forkOptions.runJVMOptions ++
      Some(mainClass) ++
      arguments
    
    val builder = Process(command, forkOptions.workingDirectory, envVars.toSeq: _*)
    val processLogger = new DebuggeeProcessLogger(listener)
    val process = builder.run(processLogger)

    new DebuggeeProcess(process)
  }

  private def fork(f: () => Unit): Unit = {
    val thread = new Thread {
      override def run(): Unit = f()
    }
    thread.start()
  }
}