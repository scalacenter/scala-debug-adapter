package ch.epfl.scala.debugadapter

import coursier._
import sbt.io.IO
import sbt.io.syntax._
import sbt.nio.file.{**, FileTreeView}
import sbt.nio.file.syntax._

import java.io.{BufferedReader, File, InputStream, InputStreamReader}
import java.net.InetSocketAddress
import java.nio.file.{Path, Paths}
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

import MainDebuggeeRunner._

case class MainDebuggeeRunner(source: Path, classpath: String, allClasses: Seq[Path], mainClass: String) extends DebuggeeRunner {
  override def name: String = mainClass
  
  override def run(listener: DebuggeeListener): CancelableFuture[Unit] = {
    val command = Array("java", DebugInterface, "-cp", classpath, mainClass)
    val builder = new ProcessBuilder(command: _*)
    val process = builder.start()
    new MainProcess(process, listener)
  }

  override def classFilesMappedTo(origin: Path, lines: Array[Int], columns: Array[Int]): List[Path] =
    allClasses.toList
}

object MainDebuggeeRunner {
  private final val DebugInterface = "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,quiet=n"
  private final val JDINotificationPrefix = "Listening for transport dt_socket at address: "
  
  def sleep(dest: File): MainDebuggeeRunner = {
    val src = getResource("/scala/Sleep.scala")
    compileScala(src, "Sleep", dest, ScalaVersion.`2.12`)
  }

  def helloWorld(dest: File): MainDebuggeeRunner = {
    val src = getResource("/scala/HelloWorld.scala")
    compileScala(src, "HelloWorld", dest, ScalaVersion.`2.12`)
  }

  def sysExit(dest: File): MainDebuggeeRunner = {
    val src = getResource("/scala/SysExit.scala")
    compileScala(src, "SysExit", dest, ScalaVersion.`2.12`)
  }

  def scalaBreakpointTest(dest: File, scalaVersion: ScalaVersion): MainDebuggeeRunner = {
    val src = getResource("/scala/BreakpointTest.scala")
    compileScala(src, "BreakpointTest", dest, scalaVersion)
  }

  def javaBreakpointTest(dest: File): MainDebuggeeRunner = {
    val src = getResource("/java/BreakpointTest.java")
    compileJava(src, "BreakpointTest", dest)
  }

  def scala3Braceless(dest: File): MainDebuggeeRunner = {
    val src = getResource("/scala3/braceless.scala")
    compileScala(src, "example.Example", dest, ScalaVersion.`3`)
  }

  def scala3MainAnnotation(dest: File): MainDebuggeeRunner = {
    val src = getResource("/scala3/main-annotation.scala")
    compileScala(src, "example.app", dest, ScalaVersion.`3`)
  }

  private def getResource(name: String): Path =
    Paths.get(getClass.getResource(name).toURI)

  private def compileScala(src: Path, mainClass: String, dest: File, scalaVersion: ScalaVersion): MainDebuggeeRunner = {
    val classDir = dest / "classes"
    IO.createDirectory(classDir)
    val compilerClasspath = fetch(scalaVersion.compiler).map(_.getAbsolutePath).mkString(File.pathSeparator)
    val libraryClasspath = fetch(scalaVersion.library).map(_.getAbsolutePath).mkString(File.pathSeparator)
    
    val command = Array(
      "java",
      "-classpath", compilerClasspath,
      scalaVersion.compilerMain,
      "-d", classDir.getAbsolutePath,
      "-classpath", libraryClasspath,
      src.toAbsolutePath.toString
    )
    val builder = new ProcessBuilder(command: _*)
    val process = builder.start()
    startCrawling(process.getInputStream)(System.out.println)
    startCrawling(process.getErrorStream)(System.err.println)

    val exitValue = process.waitFor()
    if (exitValue != 0) throw new IllegalArgumentException(s"cannot compile $src")
    
    val allClasses = FileTreeView.default
      .list(classDir.toPath.toGlob / ** / "*.class")
      .map { case (path, _) => path}
    val classPath = classDir.getAbsolutePath + File.pathSeparator + libraryClasspath
    MainDebuggeeRunner(src, classPath, allClasses, mainClass)
  }

  private def fetch(artifact: Dependency): Seq[File] = {
    coursier.Fetch()
      .addDependencies(artifact)
      .run()
  }

  private def compileJava(src: Path, mainClass: String, dest: File): MainDebuggeeRunner = {
    val classDir = dest / "classes"
    IO.createDirectory(classDir)
    val command = Array(
      "javac",
      "-d", classDir.getAbsolutePath,
      src.toAbsolutePath.toString
    )
    val builder = new ProcessBuilder(command: _*)
    val process = builder.start()
    
    startCrawling(process.getInputStream)(System.out.println)
    startCrawling(process.getErrorStream)(System.err.println)

    val exitValue = process.waitFor()
    if (exitValue != 0) throw new IllegalArgumentException(s"cannot compile $src")
    
    val allClasses = IO.listFiles(classDir).map(_.toPath).toList
    val classPath = classDir.getAbsolutePath + File.pathSeparator
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
