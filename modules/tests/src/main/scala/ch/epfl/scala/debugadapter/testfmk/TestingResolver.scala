package ch.epfl.scala.debugadapter.testfmk

import scala.collection.mutable
import coursier._
import scala.util.Try
import ch.epfl.scala.debugadapter.DebugToolsResolver
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.BuildInfo
import ch.epfl.scala.debugadapter.Library
import ch.epfl.scala.debugadapter.SourceJar
import java.io.File

object TestingResolver extends DebugToolsResolver {
  private val cache = mutable.Map.empty[ScalaVersion, ScalaInstance]

  def fetchOnly(org: String, name: String, version: String): Library = {
    fetch(org, name, version)
      .find(_.absolutePath.getFileName.toString == s"$name-$version.jar")
      .get
  }

  def fetch(org: String, name: String, version: String): Seq[Library] = {
    val dependency = Dependency(coursier.Module(Organization(org), ModuleName(name)), version)
    fetch(dependency)
  }

  def fetch(dependencies: Dependency*): Seq[Library] = {
    coursier
      .Fetch()
      .addRepositories(MavenRepository("https://repo1.maven.org/maven2/dev"))
      .addDependencies(dependencies: _*)
      .addClassifiers(Classifier.sources)
      .withMainArtifacts()
      .run()
      .groupBy(getArtifactId)
      .toSeq
      .flatMap { case (artifactId, jars) =>
        for {
          sourceJar <- jars.find(_.getName.endsWith("-sources.jar"))
          classJar <- jars.find(_ != sourceJar)
        } yield {
          val verisonFolder =
            if (classJar.getParentFile.getName == "jars") classJar.getParentFile.getParentFile
            else classJar.getParentFile
          Library(artifactId, verisonFolder.getName, classJar.toPath, Seq(SourceJar(sourceJar.toPath)))
        }
      }
  }

  private def getArtifactId(file: File): String = {
    file.getName.stripSuffix(".jar").stripSuffix("-sources")
  }

  override def resolveExpressionCompiler(scalaVersion: ScalaVersion): Try[ClassLoader] =
    Try(get(scalaVersion).expressionCompilerClassLoader)

  override def resolveUnpickler(scalaVersion: ScalaVersion): Try[ClassLoader] =
    Try(get(scalaVersion).unpicklerClassLoader)

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

    val jars = fetch(expressionCompilerDep)
    val libraryJars = jars.filter(jar => jar.name.startsWith("scala-library"))
    val expressionCompilerJar = jars.find(jar => jar.name.startsWith(expressionCompilerArtifact)).get
    val compilerJars = jars.filter(jar => !libraryJars.contains(jar) && jar != expressionCompilerJar)

    new Scala2Instance(libraryJars, compilerJars, expressionCompilerJar)
  }

  private def fetchScala3(scalaVersion: ScalaVersion): Scala3Instance = {
    val expressionCompilerArtifact =
      s"${BuildInfo.expressionCompilerName}_${scalaVersion.value}"
    val expressionCompilerDep = Dependency(
      coursier.Module(Organization(BuildInfo.organization), ModuleName(expressionCompilerArtifact)),
      BuildInfo.version
    )

    val unpicklerDep = Dependency(
      coursier.Module(Organization(BuildInfo.organization), ModuleName(s"${BuildInfo.unpicklerName}_3")),
      BuildInfo.version
    )

    val tastyDep = Dependency(
      coursier.Module(Organization("org.scala-lang"), ModuleName(s"tasty-core_3")),
      scalaVersion.value
    )

    val jars = fetch(expressionCompilerDep)
    val unpicklerJars = fetch(unpicklerDep, tastyDep)
    val libraryJars =
      jars.filter(jar => jar.name.startsWith("scala-library") || jar.name.startsWith("scala3-library_3"))
    val expressionCompilerJar = jars.find(jar => jar.name.startsWith(expressionCompilerArtifact)).get
    val compilerJars = jars.filter(jar => !libraryJars.contains(jar) && jar != expressionCompilerJar)

    new Scala3Instance(libraryJars, compilerJars, expressionCompilerJar, unpicklerJars)
  }
}
