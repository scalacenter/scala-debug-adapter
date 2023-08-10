package ch.epfl.scala.debugadapter.testfmk

import ch.epfl.scala.debugadapter.CancelableFuture
import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.DebuggeeListener
import ch.epfl.scala.debugadapter.JavaRuntime
import ch.epfl.scala.debugadapter.Library
import ch.epfl.scala.debugadapter.ManagedEntry
import ch.epfl.scala.debugadapter.Module
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.SourceDirectory
import ch.epfl.scala.debugadapter.StandaloneSourceFile
import ch.epfl.scala.debugadapter.UnmanagedEntry
import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee._

import java.io.BufferedReader
import java.io.InputStream
import java.io.InputStreamReader
import java.net.InetSocketAddress
import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.Properties
import scala.util.control.NonFatal
import io.reactivex.subjects.Subject
import io.reactivex.subjects.PublishSubject

case class TestingDebuggee(
    tempDir: Path,
    scalaVersion: ScalaVersion,
    sourceFiles: Seq[Path],
    mainModule: Module,
    dependencies: Seq[ManagedEntry],
    mainClass: String,
    javaRuntime: Option[JavaRuntime],
    scalaInstance: ScalaInstance,
    scalacOptions: Seq[String]
) extends Debuggee
    with TestingContext {

  override val classesToUpdate: Subject[Seq[String]] = PublishSubject.create()
  def mainSource: Path = sourceFiles.head

  override def name: String = mainClass
  override def modules: Seq[Module] = Seq(mainModule) ++ dependencies.collect { case m: Module => m }

  override def libraries: Seq[Library] = dependencies.collect { case m: Library => m }
  override def unmanagedEntries: Seq[UnmanagedEntry] = Seq.empty
  override def run(listener: DebuggeeListener): CancelableFuture[Unit] = {
    val cmd = Seq("java", DebugInterface, "-cp", classPathString, mainClass)
    val builder = new ProcessBuilder(cmd: _*)
    val process = builder.start()
    new MainProcess(process, listener)
  }

  lazy val classLoader: ClassLoader =
    new URLClassLoader(classPathEntries.map(_.absolutePath.toUri.toURL).toArray, null)

  def compileScala(source: String, fileName: String): Unit = {
    val sourceFile = {
      val sourceFile = tempDir.resolve("src").resolve(fileName)
      Files.write(sourceFile, source.getBytes())
      sourceFile
    }
    scalaInstance.compile(mainModule.absolutePath, dependencies, scalacOptions, Seq(sourceFile))
  }

  private[debugadapter] def getPathOf(fileName: String) =
    tempDir.resolve("src").resolve(fileName)
}

object TestingDebuggee {
  private final val DebugInterface = "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,quiet=n"
  private final val JDINotificationPrefix = "Listening for transport dt_socket at address: "

  lazy val sleep: TestingDebuggee = {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    synchronized(wait()) // block for all eternity
         |  }
         |}
         |""".stripMargin
    mainClass(source, "example.Main", ScalaVersion.`2.12`)
  }

  lazy val helloWorld: TestingDebuggee = {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    println("Hello, World!")
         |  }
         |}
         |""".stripMargin
    mainClass(source, "example.Main", ScalaVersion.`2.12`)
  }

  def scalaBreakpointTest(scalaVersion: ScalaVersion): TestingDebuggee = {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    println("Breakpoint in main method")
         |    val h = new Hello
         |    h.greet()
         |    Hello // Force initialization of constructor
         |    println("Finished all breakpoints")
         |  }
         |  class Hello {
         |    def greet(): Unit = {
         |      println("Breakpoint in hello class")
         |      class InnerHello { println("Breakpoint in hello inner class") }
         |      new InnerHello()
         |      ()
         |    }
         |
         |    override def toString: String = "hello"
         |  }
         |  object Hello {
         |    println("Breakpoint in hello object")
         |    val a = 1
         |  }
         |}
         |
         |""".stripMargin
    mainClass(source, "example.Main", scalaVersion)
  }

  private def getRuntime(withSources: Boolean = true): Option[JavaRuntime] = {
    if (withSources) JavaRuntime(javaHome)
    else JavaRuntime.java8(javaHome, None).orElse(JavaRuntime.java9OrAbove(javaHome, None))
  }

  def mainClass(source: String, mainClassName: String, scalaVersion: ScalaVersion): TestingDebuggee = {
    val className = mainClassName.split('.').last
    val sourceName = s"$className.scala"
    mainClass(Seq(sourceName -> source), mainClassName, scalaVersion, Seq.empty, Seq.empty)
  }

  def mainClassWithoutJDKSources(source: String, mainClassName: String, scalaVersion: ScalaVersion): TestingDebuggee = {
    val className = mainClassName.split('.').last
    val sourceName = s"$className.scala"
    val runtime = getRuntime(withSources = false)
    mainClass(Seq(sourceName -> source), mainClassName, scalaVersion, Seq.empty, Seq.empty, runtime)
  }

  def mainClass(sources: Seq[(String, String)], mainClassName: String, scalaVersion: ScalaVersion): TestingDebuggee =
    mainClass(sources, mainClassName, scalaVersion, Seq.empty, Seq.empty)

  def mainClass(
      source: String,
      mainClassName: String,
      scalaVersion: ScalaVersion,
      scalacOptions: Seq[String],
      dependencies: Seq[ManagedEntry]
  ): TestingDebuggee = {
    val className = mainClassName.split('.').last
    val sourceName = s"$className.scala"
    mainClass(Seq(sourceName -> source), mainClassName, scalaVersion, scalacOptions, dependencies)
  }

  def mainClass(
      source: String,
      mainClassName: String,
      scalaVersion: ScalaVersion,
      scalacOptions: Seq[String]
  ): TestingDebuggee = {
    val className = mainClassName.split('.').last
    val sourceName = s"$className.scala"
    mainClass(Seq(sourceName -> source), mainClassName, scalaVersion, scalacOptions, Seq.empty)
  }

  def mainClass(
      sources: Seq[(String, String)],
      mainClassName: String,
      scalaVersion: ScalaVersion,
      scalacOptions: Seq[String],
      dependencies: Seq[ManagedEntry],
      javaRuntime: Option[JavaRuntime] = JavaRuntime(Properties.jdkHome)
  ): TestingDebuggee = {
    val tempDir = Files.createTempDirectory("scala-debug-adapter")

    val srcDir = tempDir.resolve("src")
    Files.createDirectory(srcDir)
    val classDir = tempDir.resolve("classes")
    Files.createDirectory(classDir)

    val sourceFiles = for ((fileName, source) <- sources) yield {
      val sourceFile = srcDir.resolve(fileName)
      Files.write(sourceFile, source.getBytes())
      sourceFile
    }

    val scalaInstance = TestingResolver.get(scalaVersion)
    def isScalaLibrary(dep: ManagedEntry): Boolean = {
      if (scalaVersion.isScala3) dep.name.startsWith("scala-library") || dep.name.startsWith("scala3-library")
      else dep.name.startsWith("scala-library")
    }
    val allDependencies =
      if (dependencies.isEmpty) scalaInstance.libraryJars
      else dependencies.filter(!isScalaLibrary(_)) ++ scalaInstance.libraryJars

    scalaInstance.compile(classDir, allDependencies, scalacOptions, sourceFiles)
    val sourceEntries = sourceFiles.map { srcFile =>
      StandaloneSourceFile(srcFile, srcDir.relativize(srcFile).toString)
    }

    val mainModule = Module(mainClassName, Some(scalaVersion), scalacOptions, classDir, sourceEntries)
    TestingDebuggee(
      tempDir,
      scalaVersion,
      sourceFiles,
      mainModule,
      allDependencies,
      mainClassName,
      javaRuntime,
      scalaInstance,
      scalacOptions
    )
  }

  def munitTestSuite(
      source: String,
      testSuite: String,
      scalaVersion: ScalaVersion
  ): TestingDebuggee = {
    val tempDir = Files.createTempDirectory("scala-debug-adapter")

    val srcDir = tempDir.resolve("src")
    Files.createDirectory(srcDir)
    val outDir = tempDir.resolve("out")
    Files.createDirectory(outDir)

    val sourceFile = srcDir.resolve(s"$testSuite.scala")
    Files.write(sourceFile, source.getBytes())

    val testRunner = srcDir.resolve("TestRunner.scala")
    val testRunnerSource =
      s"""|import scala.concurrent.Await
          |import scala.util.Try
          |import scala.concurrent.duration.Duration
          |
          |object TestRunner {
          |  def main(args: Array[String]): Unit = {
          |    val suite = new $testSuite()
          |    suite.munitTests().foreach { test =>
          |      Try(Await.result(test.body(), Duration.Inf))
          |    }
          |  }
          |}
          |""".stripMargin
    Files.write(testRunner, testRunnerSource.getBytes())

    val scalaInstance = TestingResolver.get(scalaVersion)
    val classDir = outDir.resolve("classes")
    Files.createDirectory(classDir)

    val dependencies = TestingResolver.fetch(
      "org.scalameta",
      s"munit_${scalaVersion.binaryVersion}",
      "0.7.29"
    )
    val classPath = (scalaInstance.libraryJars ++ dependencies).distinct
    scalaInstance.compile(classDir, classPath, Seq.empty, Seq(sourceFile, testRunner))

    val sourceEntry = SourceDirectory(srcDir)
    val mainModule = Module(testSuite, Some(scalaVersion), Seq.empty, classDir, Seq(sourceEntry))
    TestingDebuggee(
      tempDir,
      scalaVersion,
      Seq(sourceFile),
      mainModule,
      classPath,
      "TestRunner",
      getRuntime(),
      scalaInstance,
      Seq.empty
    )
  }

  private def getResource(name: String): Path =
    Paths.get(getClass.getResource(name).toURI)

  private val isWin = Properties.isWin
  val javaHome = Paths.get(Properties.jdkHome)
  private val ext = if (isWin) ".exe" else ""
  private val java = javaHome.resolve(s"bin/java$ext")
  private val javac = javaHome.resolve(s"bin/javac$ext")

  def fromJavaSource(source: String, mainClassName: String, scalaVersion: ScalaVersion): TestingDebuggee = {
    val tempDir = Files.createTempDirectory("scala-debug-adapter")

    val srcDir = tempDir.resolve("src")
    Files.createDirectory(srcDir)
    val classDir = tempDir.resolve("classes")
    Files.createDirectory(classDir)

    val srcFile = srcDir.resolve(mainClassName.split('.').last + ".java")
    Files.write(srcFile, source.getBytes())

    val command = Array(
      javac.toString,
      "-g:source,lines,vars",
      "-d",
      classDir.toString,
      srcFile.toString
    )
    val builder = new ProcessBuilder(command: _*)
    val process = builder.start()

    startCrawling(process.getInputStream)(System.out.println)
    startCrawling(process.getErrorStream)(System.err.println)

    val exitValue = process.waitFor()
    if (exitValue != 0)
      throw new IllegalArgumentException(s"Cannot compile $srcFile")

    val sourceEntry = SourceDirectory(srcDir)
    val scalaInstance = TestingResolver.get(scalaVersion)
    val mainModule = Module(mainClassName, None, Seq.empty, classDir, Seq(sourceEntry))
    TestingDebuggee(
      tempDir,
      scalaVersion,
      Seq(srcFile),
      mainModule,
      Seq.empty,
      mainClassName,
      getRuntime(),
      scalaInstance,
      Seq.empty
    )
  }

  private def startCrawling(input: InputStream)(f: String => Unit): Unit = {
    val reader = new BufferedReader(new InputStreamReader(input))
    val thread = new Thread {
      override def run(): Unit = {
        var terminated = false
        try {
          while (!terminated) {
            val line = reader.readLine()
            if (line == null) terminated = true
            else f(line)
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
        else
          exited.failure(new Exception(s"Process exited with code $exitValue"))
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
