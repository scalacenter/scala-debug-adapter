package ch.epfl.scala.debugadapter.testfmk

import ch.epfl.scala.debugadapter.BuildInfo
import ch.epfl.scala.debugadapter.DebugToolsResolver
import ch.epfl.scala.debugadapter.Library
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.SourceJar
import coursier.*

import java.io.File
import java.nio.file.Path
import scala.collection.mutable
import scala.util.Try

case class FetchOptions(
    keepOptional: Boolean = false,
    keepProvided: Boolean = false,
    repositories: Seq[Repository] = Seq.empty,
    exclusions: Seq[(String, String)] = Seq.empty
)

object FetchOptions {
  def default = FetchOptions()
}

object TestingResolver extends DebugToolsResolver {
  private val cache = mutable.Map.empty[ScalaVersion, ScalaInstance]

  def fetchOnly(org: String, name: String, version: String): Library = {
    fetch(org, name, version)
      .find(_.absolutePath.getFileName.toString == s"$name-$version.jar")
      .get
  }

  def fetch(org: String, name: String, version: String, options: FetchOptions = FetchOptions.default): Seq[Library] = {
    val dep = Dependency(coursier.Module(Organization(org), ModuleName(name)), version)
    fetch(Seq(dep), options)
  }

  def fetch(dependencies: Dependency*): Seq[Library] = fetch(dependencies, FetchOptions.default)

  def fetch(dependencies: Seq[Dependency], options: FetchOptions): Seq[Library] = {
    coursier
      .Fetch()
      .addRepositories(
        options.repositories ++ Seq(
          MavenRepository("https://repo1.maven.org/maven2/dev"),
          MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
        ): _*
      )
      .addDependencies(dependencies: _*)
      .addClassifiers(Classifier.sources)
      .withMainArtifacts()
      .mapResolutionParams { params =>
        val exclusions = options.exclusions.map { case (org, mod) => (Organization(org), ModuleName(mod)) }.toSet
        params
          .withKeepOptionalDependencies(options.keepOptional)
          // .withKeepProvidedDependencies(options.keepProvided)
          .withExclusions(exclusions)
      }
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

  override def resolveDecoder(scalaVersion: ScalaVersion): Try[Seq[Path]] =
    Try(get(scalaVersion).decoderJars.map(_.absolutePath))

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
    val builtInExpressionCompiler = scalaVersion.isScala3 && scalaVersion.minor >= 7
    val (expressionCompilerOrg, expressionCompilerArtifact, expressionCompilerVersion) =
      if (builtInExpressionCompiler) {
        ("org.scala-lang", s"scala3-compiler_3", scalaVersion.value)
      } else {
        (BuildInfo.organization, s"${BuildInfo.expressionCompilerName}_${scalaVersion.value}", BuildInfo.version)
      }
    val expressionCompilerDep = Dependency(
      coursier.Module(Organization(expressionCompilerOrg), ModuleName(expressionCompilerArtifact)),
      expressionCompilerVersion
    )

    val decoderDep = Dependency(
      coursier.Module(Organization(BuildInfo.organization), ModuleName(s"${BuildInfo.decoderName}_3")),
      BuildInfo.version
    )

    val tastyDep = Dependency(
      coursier.Module(Organization("org.scala-lang"), ModuleName(s"tasty-core_3")),
      scalaVersion.value
    )

    val jars = fetch(expressionCompilerDep)
    val decoderJars = fetch(decoderDep, tastyDep)
    val libraryJars =
      jars.filter(jar => jar.name.startsWith("scala-library") || jar.name.startsWith("scala3-library_3"))
    val expressionCompilerJar =
      if (builtInExpressionCompiler) None
      else Some(jars.find(jar => jar.name.startsWith(expressionCompilerArtifact)).get)
    val compilerJars =
      jars.filter(jar => !libraryJars.contains(jar) && !expressionCompilerJar.exists(_ == jar))

    new Scala3Instance(libraryJars, compilerJars, expressionCompilerJar, decoderJars)
  }
}
