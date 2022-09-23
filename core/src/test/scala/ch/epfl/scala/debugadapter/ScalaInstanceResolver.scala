package ch.epfl.scala.debugadapter

import scala.collection.mutable
import java.net.URLClassLoader
import coursier._
import java.nio.file.Path
import java.io.File
import scala.util.Try

sealed abstract class ScalaInstance(
    val libraryJars: Seq[Library],
    compilerJars: Seq[Library],
    expressionCompilerJar: Library,
    stepFilterJars: Seq[Library]
) {
  val libraryClassLoader = new URLClassLoader(libraryJars.map(_.toURL).toArray, null)
  val compilerClassLoader = new URLClassLoader(compilerJars.map(_.toURL).toArray, libraryClassLoader)
  val expressionCompilerClassLoader = new URLClassLoader(Array(expressionCompilerJar.toURL), compilerClassLoader)
  val stepFilterClassLoader = new URLClassLoader(stepFilterJars.map(_.toURL).toArray, null)

  def compile(classDir: Path, classPath: Seq[ClassPathEntry], sourceFiles: Seq[Path]): Unit = {
    val args = Array(
      "-d",
      classDir.toString,
      "-classpath",
      classPath.map(_.absolutePath).mkString(File.pathSeparator),
      "-deprecation"
    ) ++ sourceFiles.map(_.toString)
    compileInternal(args)
  }

  protected def compileInternal(args: Array[String]): Unit
}

final class Scala2Instance(
    libraryJars: Seq[Library],
    compilerJars: Seq[Library],
    expressionCompilerJar: Library,
    stepFilterJars: Seq[Library]
) extends ScalaInstance(libraryJars, compilerJars, expressionCompilerJar, stepFilterJars) {
  override protected def compileInternal(args: Array[String]): Unit = {
    val main = compilerClassLoader.loadClass("scala.tools.nsc.Main")
    val process = main.getMethod("process", classOf[Array[String]])
    val success = process.invoke(null, args).asInstanceOf[Boolean]
    if (!success) throw new Exception("compilation failed")
  }
}

final class Scala3Instance(
    libraryJars: Seq[Library],
    compilerJars: Seq[Library],
    expressionCompilerJar: Library,
    stepFilterJars: Seq[Library]
) extends ScalaInstance(libraryJars, compilerJars, expressionCompilerJar, stepFilterJars) {
  override protected def compileInternal(args: Array[String]): Unit = {
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

object ScalaInstanceResolver extends DebugToolsResolver {
  private val cache = mutable.Map.empty[ScalaVersion, ScalaInstance]

  override def resolveExpressionCompiler(scalaVersion: ScalaVersion): Try[ClassLoader] =
    Try(get(scalaVersion).expressionCompilerClassLoader)

  override def resolveStepFilter(scalaVersion: ScalaVersion): Try[ClassLoader] =
    Try(get(scalaVersion).stepFilterClassLoader)

  def get(scalaVersion: ScalaVersion): ScalaInstance = {
    if (!cache.contains(scalaVersion)) {
      val scalaCompiler =
        if (scalaVersion.isScala2) fetchScala2(scalaVersion)
        else fetchScala3(scalaVersion)
      cache.update(scalaVersion, scalaCompiler)
    }
    cache(scalaVersion)
  }

  private def fetchScala2(scalaVersion: ScalaVersion): Scala2Instance = {
    val expressionCompilerArtifact = s"${BuildInfo.expressionCompilerName}_${scalaVersion.value}"
    val expressionCompilerDep = Dependency(
      coursier.Module(Organization(BuildInfo.organization), ModuleName(expressionCompilerArtifact)),
      BuildInfo.version
    )

    val jars = Coursier.fetch(expressionCompilerDep)
    val libraryJars = jars.filter(jar => jar.name.startsWith("scala-library"))
    val expressionCompilerJar = jars.find(jar => jar.name.startsWith(expressionCompilerArtifact)).get
    val compilerJars = jars.filter(jar => !libraryJars.contains(jar) && jar != expressionCompilerJar)

    new Scala2Instance(libraryJars, compilerJars, expressionCompilerJar, Seq.empty)
  }

  private def fetchScala3(scalaVersion: ScalaVersion): Scala3Instance = {
    val expressionCompilerArtifact =
      s"${BuildInfo.expressionCompilerName}_${scalaVersion.value}"
    val expressionCompilerDep = Dependency(
      coursier.Module(Organization(BuildInfo.organization), ModuleName(expressionCompilerArtifact)),
      BuildInfo.version
    )

    val stepFilterDep = Dependency(
      coursier.Module(Organization(BuildInfo.organization), ModuleName(s"${BuildInfo.scala3StepFilterName}_3")),
      BuildInfo.version
    )

    val tastyDep = Dependency(
      coursier.Module(Organization("org.scala-lang"), ModuleName(s"tasty-core_3")),
      scalaVersion.value
    )

    val jars = Coursier.fetch(expressionCompilerDep)
    val stepFilterJars = Coursier.fetch(stepFilterDep, tastyDep)
    val libraryJars =
      jars.filter(jar => jar.name.startsWith("scala-library") || jar.name.startsWith("scala3-library_3"))
    val expressionCompilerJar = jars.find(jar => jar.name.startsWith(expressionCompilerArtifact)).get
    val compilerJars = jars.filter(jar => !libraryJars.contains(jar) && jar != expressionCompilerJar)

    new Scala3Instance(libraryJars, compilerJars, expressionCompilerJar, stepFilterJars)
  }
}
