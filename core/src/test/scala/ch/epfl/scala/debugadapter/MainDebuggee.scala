package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.MainDebuggee._

import java.io.{BufferedReader, File, InputStream, InputStreamReader}
import java.nio.file.{Files, Path, Paths}
import scala.concurrent.{Future, Promise}
import scala.util.Properties
import scala.util.control.NonFatal
import java.net.InetSocketAddress

case class MainDebuggee(
    scalaVersion: ScalaVersion,
    sourceFiles: Seq[Path],
    mainModule: Module,
    libraries: Seq[Library],
    mainClass: String
) extends Debuggee {
  override def name: String = mainClass
  override def modules: Seq[Module] = Seq(mainModule)
  override def unmanagedEntries: Seq[UnmanagedEntry] = Seq.empty
  override def run(listener: DebuggeeListener): CancelableFuture[Unit] = {
    val cmd = Seq(
      "java",
      DebugInterface,
      "-cp",
      classPath.mkString(File.pathSeparator),
      mainClass
    )
    val builder = new ProcessBuilder(cmd: _*)
    val process = builder.start()
    new MainProcess(process, listener)
  }
  override def javaRuntime: Option[JavaRuntime] = JavaRuntime(javaHome)
}

object MainDebuggee {
  private final val DebugInterface =
    "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,quiet=n"
  private final val JDINotificationPrefix =
    "Listening for transport dt_socket at address: "

  def sleep(): MainDebuggee =
    fromResource(
      "/scala/Sleep.scala",
      "scaladebug.test.Sleep",
      ScalaVersion.`2.12`
    )

  def helloWorld(): MainDebuggee =
    fromResource(
      "/scala/HelloWorld.scala",
      "scaladebug.test.HelloWorld",
      ScalaVersion.`2.12`
    )

  def sysExit(): MainDebuggee =
    fromResource(
      "/scala/SysExit.scala",
      "scaladebug.test.SysExit",
      ScalaVersion.`2.12`
    )

  def scalaBreakpointTest(
      scalaVersion: ScalaVersion
  ): MainDebuggee =
    fromResource(
      "/scala/BreakpointTest.scala",
      "scaladebug.test.BreakpointTest",
      scalaVersion
    )

  def javaBreakpointTest(): MainDebuggee = {
    val file = getResource("/java/BreakpointTest.java")
    val source = new String(Files.readAllBytes(file))
    fromJavaSource(
      ScalaVersion.`2.12`,
      source,
      "scaladebug.test.BreakpointTest"
    )
  }

  def scala3Braceless(): MainDebuggee =
    fromResource(
      "/scala-3/braceless.scala",
      "scaladebug.test.Example",
      ScalaVersion.`3.0`
    )

  def scala3MainAnnotation(): MainDebuggee =
    fromResource(
      "/scala-3/main-annotation.scala",
      "scaladebug.test.app",
      ScalaVersion.`3.0`
    )

  def fromResource(
      resource: String,
      mainClass: String,
      scalaVersion: ScalaVersion
  ): MainDebuggee = {
    val file = getResource(resource)
    val source = new String(Files.readAllBytes(file))
    mainClassRunner(source, mainClass, scalaVersion)
  }

  private def munitTestRunner(testSuite: String) =
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

  def munitTestSuite(
      source: String,
      testSuite: String,
      scalaVersion: ScalaVersion
  ): MainDebuggee = {
    val tempDir = Files.createTempDirectory("scala-debug-adapter")

    val srcDir = tempDir.resolve("src")
    Files.createDirectory(srcDir)
    val outDir = tempDir.resolve("out")
    Files.createDirectory(outDir)

    val sourceFile = srcDir.resolve(s"$testSuite.scala")
    Files.write(sourceFile, source.getBytes())

    val testRunner = srcDir.resolve("TestRunner.scala")
    Files.write(testRunner, munitTestRunner(testSuite).getBytes())

    val scalaInstance = ScalaInstanceResolver.get(scalaVersion)
    val classDir = outDir.resolve("classes")
    Files.createDirectory(classDir)

    val dependencies = Coursier.fetch(
      "org.scalameta",
      s"munit_${scalaVersion.binaryVersion}",
      "0.7.29"
    )
    val classPath = (scalaInstance.libraryJars ++ dependencies).distinct
    scalaInstance.compile(classDir, classPath, Seq(sourceFile, testRunner))

    val sourceEntry = SourceDirectory(srcDir)
    val mainModule = Module(testSuite, Some(scalaVersion), Seq.empty, classDir, Seq(sourceEntry))
    MainDebuggee(scalaVersion, Seq(sourceFile), mainModule, classPath, "TestRunner")
  }

  def mainClassRunner(
      sources: Seq[(String, String)],
      mainClass: String,
      scalaVersion: ScalaVersion,
      dependencies: Seq[Library]
  ): MainDebuggee = {
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

    val scalaInstance = ScalaInstanceResolver.get(scalaVersion)
    val libraries =
      if (dependencies.isEmpty) scalaInstance.libraryJars else dependencies

    scalaInstance.compile(classDir, libraries, sourceFiles)
    val sourceEntries = sourceFiles.map { srcFile =>
      StandaloneSourceFile(srcFile, srcDir.relativize(srcFile).toString)
    }

    val mainModule = Module(mainClass, Some(scalaVersion), Seq.empty, classDir, sourceEntries)
    MainDebuggee(scalaVersion, sourceFiles, mainModule, libraries, mainClass)
  }

  def mainClassRunner(
      source: String,
      mainClass: String,
      scalaVersion: ScalaVersion,
      dependencies: Seq[Library]
  ): MainDebuggee = {
    val className = mainClass.split('.').last
    val sourceName = s"$className.scala"
    mainClassRunner(Seq(sourceName -> source), mainClass, scalaVersion, dependencies)
  }

  def mainClassRunner(source: String, mainClass: String, scalaVersion: ScalaVersion): MainDebuggee = {
    val className = mainClass.split('.').last
    val sourceName = s"$className.scala"
    mainClassRunner(Seq(sourceName -> source), mainClass, scalaVersion, Seq.empty)
  }

  private def getResource(name: String): Path =
    Paths.get(getClass.getResource(name).toURI)

  private val isWin = Properties.isWin
  val javaHome = Paths.get(Properties.jdkHome)
  private val ext = if (isWin) ".exe" else ""
  private val java = javaHome.resolve(s"bin/java$ext")
  private val javac = javaHome.resolve(s"bin/javac$ext")

  private def fromJavaSource(
      scalaVersion: ScalaVersion,
      source: String,
      mainClass: String
  ): MainDebuggee = {
    val tempDir = Files.createTempDirectory("scala-debug-adapter")

    val srcDir = tempDir.resolve("src")
    Files.createDirectory(srcDir)
    val classDir = tempDir.resolve("classes")
    Files.createDirectory(classDir)

    val srcFile = srcDir.resolve(s"$mainClass.java")
    Files.write(srcFile, source.getBytes())

    val command = Array(
      javac.toString,
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
    val mainModule = Module(mainClass, None, Seq.empty, classDir, Seq(sourceEntry))
    MainDebuggee(scalaVersion, Seq(srcFile), mainModule, Seq.empty, mainClass)
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
