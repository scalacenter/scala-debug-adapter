package dap

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.ExecutionContext
import java.io.InputStream
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.util.control.NonFatal
import java.net.InetSocketAddress
import sbt.ForkOptions
import sbt.Fork
import java.io.File
import sbt.internal.util.Util
import sbt.io.syntax._
import java.lang.ProcessBuilder.Redirect
import scala.sys.process.Process
import sbt.OutputStrategy.StdoutOutput
import sbt.OutputStrategy
import sbt.OutputStrategy.BufferedOutput
import sbt.OutputStrategy.LoggedOutput
import sbt.OutputStrategy.CustomOutput
import scala.sys.process.ProcessLogger

class DebuggeeProcess(process: Process) extends CancelableFuture[Unit] {
  import DebuggeeProcess._
  private val exited = Promise[Unit]()

  fork { () =>
    val exitCode = process.exitValue()
    if (exitCode != 0)
      exited.failure(new Exception(s"""Nonzero exit code returned: $exitCode""".stripMargin))
    else exited.success(())
  }

  override def future: Future[Unit] = exited.future
  override def cancel(): Unit = process.destroy()
}

object DebuggeeProcess {
  def start(
    forkOptions: ForkOptions,
    mainClass: Option[String],
    arguments: Seq[String],
    callbacks: DebugSessionCallbacks
  ): DebuggeeProcess = {
    val javaHome = forkOptions.javaHome.getOrElse(new File(System.getProperty("java.home")))
    val javaBin = (javaHome / "bin" / "java").getAbsolutePath
    val (classpathEnv, options) = fitClassPath(forkOptions.runJVMOptions ++ mainClass ++ arguments)
    val command = javaBin +: options

    val envVars = forkOptions.envVars ++ classpathEnv.map("CLASSPATH" -> _)
    
    val builder = Process(command, forkOptions.workingDirectory, envVars.toSeq: _*)
    val processLogger = new DebuggeeProcessLogger(callbacks)
    val process = builder.run(processLogger)

    new DebuggeeProcess(process)
  }

  private def fork(f: () => Unit): Unit = {
    val thread = new Thread {
      override def run(): Unit = f()
    }
    thread.start()
  }

  private def fitClassPath(options: Seq[String]): (Option[String], Seq[String]) = {
    val commandSize =  options.mkString(" ").length
    val i = options.indexWhere(opt => opt == "-classpath" || opt == "-cp")
    // the windows command line size is limited
    // in that case we move the class path to a environement variable
    if (Util.isWindows && commandSize > 5000 && i != -1) {
      val classpathOption = Some(options(i + 1))
      val newOptions = options.take(i) ++ options.drop(i + 2)
      (classpathOption, newOptions)
    } else (None, options)
  } 
}