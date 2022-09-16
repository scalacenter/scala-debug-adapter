package ch.epfl.scala.debugadapter

import scala.collection.mutable
import java.net.URLClassLoader
import coursier._
import java.nio.file.Path
import java.io.File
import java.nio.file.Files

case class ScalaInstance(
    scalaVersion: ScalaVersion,
    libraryJars: Seq[Path],
    compilerJars: Seq[Path]
) {
  val libraryClassLoader =
    new URLClassLoader(libraryJars.map(_.toUri.toURL).toArray, null)
  val compilerClassLoader =
    new URLClassLoader(
      compilerJars.map(_.toUri.toURL).toArray,
      libraryClassLoader
    )

  def compile(
      classDir: Path,
      classPath: Seq[Path],
      sourceFiles: Seq[Path]
  ): Unit = {
    val args = Array(
      "-d",
      classDir.toString,
      "-classpath",
      classPath.mkString(File.pathSeparator),
      "-deprecation"
    ) ++ sourceFiles.map(_.toString)
    scalaVersion match {
      case Scala2(_) => compileScala2(args)
      case Scala3(_) => compileScala3(args)
    }
  }

  private def compileScala2(args: Array[String]): Unit = {
    val main = compilerClassLoader.loadClass("scala.tools.nsc.Main")
    val process = main.getMethod("process", classOf[Array[String]])
    val success = process.invoke(null, args).asInstanceOf[Boolean]
    if (!success) throw new Exception("compilation failed")
  }

  private def compileScala3(args: Array[String]): Unit = {
    val main = compilerClassLoader.loadClass("dotty.tools.dotc.Main")
    val process = main.getMethod("process", classOf[Array[String]])
    val classOfReporter =
      compilerClassLoader.loadClass("dotty.tools.dotc.reporting.Reporter")
    val hasErrors = classOfReporter.getMethod("hasErrors")
    val reporter = process.invoke(null, args)
    val success = !(hasErrors.invoke(reporter).asInstanceOf[Boolean])
    if (!success) throw new Exception("compilation failed")
  }
}

object ScalaInstanceCache {
  private val cache = mutable.Map.empty[ScalaVersion, ScalaInstance]

  def get(scalaVersion: ScalaVersion): ScalaInstance = {
    if (!cache.contains(scalaVersion)) {
      val scalaInstance = scalaVersion match {
        case scala2: Scala2 => fetch(scala2)
        case scala3: Scala3 => fetch(scala3)
      }
      cache.update(scalaVersion, scalaInstance)
    }
    cache(scalaVersion)
  }

  def compile(source: String, scalaVersion: ScalaVersion): Seq[Path] =
    compile(Seq("source.scala" -> source), scalaVersion, Seq.empty[Path])

  def compile(
      sources: Seq[(String, String)],
      scalaVersion: ScalaVersion,
      libraries: Seq[Path]
  ): Seq[Path] =
    val tempDir = Files.createTempDirectory("scala-debug-adapter")

    val srcDir = tempDir.resolve("src")
    Files.createDirectory(srcDir)
    val classDir = tempDir.resolve("classes")
    Files.createDirectory(classDir)

    val sourceFiles = for (fileName, source) <- sources yield
      val sourceFile = srcDir.resolve(fileName)
      Files.write(sourceFile, source.getBytes())
      sourceFile

    val scalaInstance = get(scalaVersion)
    val dependencies =
      if libraries.isEmpty then scalaInstance.libraryJars else libraries

    scalaInstance.compile(classDir, dependencies, sourceFiles)
    dependencies :+ classDir

  private def fetch(scalaVersion: Scala2): ScalaInstance = {
    val compilerArtifactName = "scala-compiler"
    val compilerDep = Dependency(
      Module(
        Organization("org.scala-lang"),
        ModuleName(compilerArtifactName)
      ),
      scalaVersion.version
    )

    val jars = Coursier.fetch(compilerDep)

    val libraryJars =
      jars.filter(jar => jar.getFileName.toString.startsWith("scala-library"))
    val compilerJars = jars.filter(jar => !libraryJars.contains(jar))

    ScalaInstance(scalaVersion, libraryJars, compilerJars)
  }

  private def fetch(scalaVersion: Scala3): ScalaInstance = {
    val compilerArtifactName = "scala3-compiler_3"
    val compilerDep = Dependency(
      Module(
        Organization("org.scala-lang"),
        ModuleName(compilerArtifactName)
      ),
      scalaVersion.version
    )

    val jars = Coursier.fetch(compilerDep)

    val libraryJars = jars.filter { jar =>
      jar.getFileName.toString.startsWith("scala-library") ||
      jar.getFileName.toString.startsWith("scala3-library_3")
    }
    val compilerJars = jars.filter(jar => !libraryJars.contains(jar))

    ScalaInstance(
      scalaVersion,
      libraryJars,
      compilerJars
    )
  }
}
