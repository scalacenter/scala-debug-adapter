package ch.epfl.scala.debugadapter

import java.nio.file.Path
import scala.tools.nsc.Main
import java.io.File
import sbt.io.syntax._
import sbt.io.IO
import buildinfo.BuildInfo
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.concurrent.Future
import scala.concurrent.Promise
import java.net.InetSocketAddress
import scala.concurrent.ExecutionContext
import MainDebuggeeRunner._
import java.nio.file.Paths
import scala.util.control.NonFatal
import java.io.InputStream
import java.util.concurrent.atomic.AtomicBoolean

case class MainDebuggeeRunner(source: Path, classpath: String, allClasses: List[Path], mainClass: String) extends DebuggeeRunner {
  override def name: String = mainClass
  
  override def run(listener: DebuggeeListener): CancelableFuture[Unit] = {
    val command = Array("java", DebugInterface, "-cp", classpath, mainClass)
    val builder = new ProcessBuilder(command: _*)
    val process = builder.start()
    new MainProcess(process, listener)
  }

  override def classFilesMappedTo(origin: Path, lines: Array[Int], columns: Array[Int]): List[Path] = allClasses
}

object MainDebuggeeRunner {
  private final val DebugInterface = "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,quiet=n"
  private final val JDINotificationPrefix = "Listening for transport dt_socket at address: "
  
  def sleep(dest: File): MainDebuggeeRunner = {
    val src = getResource("/Sleep.scala")
    compileScala(src, "Sleep", dest)
  }

  def helloWorld(dest: File): MainDebuggeeRunner = {
    val src = getResource("/HelloWorld.scala")
    compileScala(src, "HelloWorld", dest)
  }

  def sysExit(dest: File): MainDebuggeeRunner = {
    val src = getResource("/SysExit.scala")
    compileScala(src, "SysExit", dest)
  }

  def scalaBreakpointTest(dest: File): MainDebuggeeRunner = {
    val src = getResource("/BreakpointTest.scala")
    compileScala(src, "BreakpointTest", dest)
  }

  def javaBreakpointTest(dest: File): MainDebuggeeRunner = {
    val src = getResource("/BreakpointTest.java")
    compileJava(src, "BreakpointTest", dest)
  }

  def evaluateTest(dest: File): MainDebuggeeRunner = {
    val src = getResource("/EvaluateTest.scala")
    compileScala(src, "EvaluateTest", dest)
  }

  private def getResource(name: String): Path =
    Paths.get(getClass.getResource(name).toURI)

  private def compileScala(src: Path, mainClass: String, dest: File): MainDebuggeeRunner = {
    val classDir = dest / "classes"
    IO.createDirectory(classDir)
    val args = Array(
      "-d", classDir.getAbsolutePath,
      "-classpath", BuildInfo.scalaLibraries,
      src.toAbsolutePath.toString
    )
    val success = Main.process(args)
    if (!success) throw new IllegalArgumentException(s"cannot compile $src")
    val allClasses = IO.listFiles(classDir).map(_.toPath).toList
    val classPath = classDir.getAbsolutePath + File.pathSeparator + BuildInfo.scalaLibraries
    MainDebuggeeRunner(src, classPath, allClasses, mainClass)
  }

  private def compileJava(src: Path, mainClass: String, dest: File): MainDebuggeeRunner = {
    val classDir = dest / "classes"
    IO.createDirectory(classDir)
    val command = Array(
      "javac",
      "-d", classDir.getAbsolutePath,
      "-classpath", BuildInfo.scalaLibraries,
      src.toAbsolutePath.toString
    )
    val builder = new ProcessBuilder(command: _*)
    val process = builder.start()
    
    startCrawling(process.getInputStream)(System.out.println)
    startCrawling(process.getErrorStream)(System.err.println)

    val exitValue = process.waitFor()
    if (exitValue != 0) throw new IllegalArgumentException(s"cannot compile $src")
    
    val allClasses = IO.listFiles(classDir).map(_.toPath).toList
    val classPath = classDir.getAbsolutePath + File.pathSeparator + BuildInfo.scalaLibraries
    new MainDebuggeeRunner(src, classPath, allClasses, mainClass)
  }

  private def startCrawling(input: InputStream)(f: String => Unit): Unit = {
    val reader = new BufferedReader(new InputStreamReader(input))
    val thread = new Thread {
      override def run(): Unit = {
        var terminated = false
        try {
          while (!terminated) {
            val line = reader.readLine()
            if (line == null) {
              terminated = true
            } else {
              f(line)
            }
          }
          input.close()
        } catch {
          case NonFatal(_) => ()
        }
      }
    }
    thread.start()
  }

  private class MainProcess(
    process: Process,
    listener: DebuggeeListener
  ) extends CancelableFuture[Unit] {
    private val exited = Promise[Unit]()

    startCrawling(process.getInputStream) { line =>
      if (line.startsWith(JDINotificationPrefix)) {
        val port = Integer.parseInt(line.drop(JDINotificationPrefix.length))
        val address = new InetSocketAddress("127.0.0.1", port)
        listener.onListening(address)
      } else {
        listener.out(line)
      }
    }
    startCrawling(process.getErrorStream)(listener.err)
    
    private val thread = new Thread {
      override def run(): Unit = {
        val exitValue = process.waitFor()
        if (exitValue == 0) exited.success(())
        else exited.failure(new Exception(s"Process exited with code $exitValue"))
      }
    }
    thread.start()

    override def future: Future[Unit] = {
      exited.future
    }
    override def cancel(): Unit = {
      if (process.isAlive) process.destroy()
    }
  }
}
