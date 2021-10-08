package ch.epfl.scala.debugadapter

import scala.collection.mutable
import java.net.URLClassLoader
import coursier._
import java.net.URL
import java.nio.file.Path
import java.io.File

case class ScalaInstance(
    scalaVersion: ScalaVersion,
    libraryJars: Seq[ClassPathEntry],
    compilerJars: Seq[ClassPathEntry],
    expressionCompilerJar: Option[ClassPathEntry]
) {
  val libraryClassLoader =
    new URLClassLoader(libraryJars.map(_.toURL).toArray, null)
  val compilerClassLoader =
    new URLClassLoader(compilerJars.map(_.toURL).toArray, libraryClassLoader)
  val expressionCompilerClassLoader = {
    expressionCompilerJar
      .map(jar => new URLClassLoader(Array(jar.toURL), compilerClassLoader))
      .getOrElse(compilerClassLoader)
  }

  def compile(classDir: Path, sourceFile: Path): Unit = {
    val args = Array(
      "-d",
      classDir.toString,
      "-classpath",
      libraryJars.map(_.absolutePath).mkString(File.pathSeparator),
      sourceFile.toString
    )
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

  private def fetch(scalaVersion: Scala2): ScalaInstance = {
    val artifactName =
      s"${BuildInfo.expressionCompilerName}_${scalaVersion.version}"
    val dependency = Dependency(
      Module(
        Organization(BuildInfo.expressionCompilerOrganization),
        ModuleName(artifactName)
      ),
      BuildInfo.expressionCompilerVersion
    )
    val jars = Coursier.fetch(dependency)

    val libraryJars = jars.filter(jar => jar.name.startsWith("scala-library"))
    val expressionCompilerJar =
      jars.find(jar => jar.name.startsWith(artifactName)).get
    val compilerJars = jars.filter(jar =>
      !libraryJars.contains(jar) && jar != expressionCompilerJar
    )

    ScalaInstance(
      scalaVersion,
      libraryJars,
      compilerJars,
      Some(expressionCompilerJar)
    )
  }

  private def fetch(scalaVersion: Scala3): ScalaInstance = {
    val artifactName = "scala3-compiler_3"
    val dependency = Dependency(
      Module(
        Organization("org.scala-lang"),
        ModuleName(artifactName)
      ),
      scalaVersion.version
    )
    val jars = Coursier.fetch(dependency)

    val libraryJars = jars.filter { jar =>
      jar.name.startsWith("scala-library") ||
      jar.name.startsWith("scala3-library")
    }
    val compilerJars = jars.filter(jar => !libraryJars.contains(jar))

    ScalaInstance(scalaVersion, libraryJars, compilerJars, None)
  }
}
