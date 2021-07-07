package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.MainDebuggeeRunner._
import sbt.io.IO
import sbt.io.syntax._

import java.io.{BufferedReader, File, InputStream, InputStreamReader}
import java.net.InetSocketAddress
import java.nio.file.{Path, Paths}
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

case class MainDebuggeeRunner(source: Path, classPathEntries: Seq[ClassPathEntry], mainClass: String) extends DebuggeeRunner {
  override def name: String = mainClass
  
  override def run(listener: DebuggeeListener): CancelableFuture[Unit] = {
    val cmd = Seq("java", DebugInterface, "-cp", classPath.mkString(File.pathSeparator), mainClass)
    val builder = new ProcessBuilder(cmd: _*)
    val process = builder.start()
    new MainProcess(process, listener)
  }
}

object MainDebuggeeRunner {
  private final val DebugInterface = "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,quiet=n"
  private final val JDINotificationPrefix = "Listening for transport dt_socket at address: "
  
  def sleep(dest: File): MainDebuggeeRunner = {
    val src = getResource("/scala/Sleep.scala")
    compileScala(src, "scaladebug.test.Sleep", dest, ScalaVersion.`2.12`)
  }

  def helloWorld(dest: File): MainDebuggeeRunner = {
    val src = getResource("/scala/HelloWorld.scala")
    compileScala(src, "scaladebug.test.HelloWorld", dest, ScalaVersion.`2.12`)
  }

  def sysExit(dest: File): MainDebuggeeRunner = {
    val src = getResource("/scala/SysExit.scala")
    compileScala(src, "scaladebug.test.SysExit", dest, ScalaVersion.`2.12`)
  }

  def scalaBreakpointTest(dest: File, scalaVersion: ScalaVersion): MainDebuggeeRunner = {
    val src = getResource("/scala/BreakpointTest.scala")
    compileScala(src, "scaladebug.test.BreakpointTest", dest, scalaVersion)
  }

  def javaBreakpointTest(dest: File): MainDebuggeeRunner = {
    val src = getResource("/java/BreakpointTest.java")
    compileJava(src, "scaladebug.test.BreakpointTest", dest)
  }

  def scala3Braceless(dest: File): MainDebuggeeRunner = {
    val src = getResource("/scala3/braceless.scala")
    compileScala(src, "scaladebug.test.Example", dest, ScalaVersion.`3`)
  }

  def scala3MainAnnotation(dest: File): MainDebuggeeRunner = {
    val src = getResource("/scala3/main-annotation.scala")
    compileScala(src, "scaladebug.test.app", dest, ScalaVersion.`3`)
  }

  private def getResource(name: String): Path =
    Paths.get(getClass.getResource(name).toURI)

  private def compileScala(src: Path, mainClass: String, dest: File, scalaVersion: ScalaVersion): MainDebuggeeRunner = {
    val classDir = dest / "classes"
    IO.createDirectory(classDir)
    val compilerClassPath = Coursier.fetch(scalaVersion.compiler)
    val libraryClassPath = Coursier.fetch(scalaVersion.library)
    
    val command = Array(
      "java",
      "-classpath",
      compilerClassPath.map(_.absolutePath).mkString(File.pathSeparator),
      scalaVersion.compilerMain,
      "-d", classDir.getAbsolutePath,
      "-classpath",
      libraryClassPath.map(_.absolutePath).mkString(File.pathSeparator),
      src.toAbsolutePath.toString
    )
    val builder = new ProcessBuilder(command: _*)
    val process = builder.start()
    startCrawling(process.getInputStream)(System.out.println)
    startCrawling(process.getErrorStream)(System.err.println)

    val exitValue = process.waitFor()
    if (exitValue != 0) throw new IllegalArgumentException(s"cannot compile $src")
    
    val sourceEntry = StandaloneSourceFile(src, src.getParent.relativize(src).toString)
    val mainClassPathEntry = ClassPathEntry(classDir.toPath, Seq(sourceEntry))
    MainDebuggeeRunner(src, libraryClassPath :+ mainClassPathEntry, mainClass)
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

    val sourceEntry = StandaloneSourceFile(src, src.getParent.relativize(src).toString)
    val mainClassPathEntry = ClassPathEntry(classDir.toPath, Seq(sourceEntry))
    new MainDebuggeeRunner(src, Seq(mainClassPathEntry), mainClass)
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
