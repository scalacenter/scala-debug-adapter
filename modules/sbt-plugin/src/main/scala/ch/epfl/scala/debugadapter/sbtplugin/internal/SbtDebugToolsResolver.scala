package ch.epfl.scala.debugadapter.sbtplugin.internal

import ch.epfl.scala.debugadapter.DebugToolsResolver
import ch.epfl.scala.debugadapter.ScalaVersion
import sbt.librarymanagement.DependencyResolution
import sbt.librarymanagement.UpdateConfiguration
import sbt.librarymanagement.UnresolvedWarningConfiguration
import sbt.librarymanagement.ModuleID
import sbt.librarymanagement.UpdateReport
import sbt.io.Hash
import scala.util.Success
import scala.util.Try
import scala.util.Failure
import ch.epfl.scala.debugadapter.BuildInfo
import sbt.{ScalaVersion => _, _}
import java.net.URLClassLoader
import xsbti.compile.ScalaInstance

class SbtDebugToolsResolver(
    scalaInstance: ScalaInstance,
    dependencyRes: DependencyResolution,
    updateConfig: UpdateConfiguration,
    warningConfig: UnresolvedWarningConfiguration,
    logger: xsbti.Logger
) extends DebugToolsResolver {

  override def resolveExpressionCompiler(scalaVersion: ScalaVersion): Try[ClassLoader] = {
    val org = BuildInfo.organization
    val artifact = s"${BuildInfo.expressionCompilerName}_$scalaVersion"
    val version = BuildInfo.version

    for (report <- fetchArtifactsOf(org % artifact % version, Seq.empty))
      yield
        if (scalaInstance.version == scalaVersion.value) {
          val expressionCompilerJars = report
            .select(
              configurationFilter(Runtime.name),
              moduleFilter(org, artifact, version) | moduleFilter(
                "org.scala-lang.modules",
                "scala-collection-compat_2.12"
              ),
              artifactFilter(extension = "jar", classifier = "")
            )
            .map(_.toURI.toURL)
            .toArray
          new URLClassLoader(expressionCompilerJars, scalaInstance.loader)
        } else {
          val expressionCompilerJars = report
            .select(
              configurationFilter(Runtime.name),
              moduleFilter(),
              artifactFilter(extension = "jar", classifier = "")
            )
            .map(_.toURI.toURL)
            .toArray
          new URLClassLoader(expressionCompilerJars, null)
        }
  }

  override def resolveDecoder(scalaVersion: ScalaVersion): Try[ClassLoader] = {
    val org = BuildInfo.organization
    val artifact = s"${BuildInfo.decoderName}_3"
    val version = BuildInfo.version
    val tastyDep = "org.scala-lang" % "tasty-core_3" % scalaVersion.value

    for (report <- fetchArtifactsOf(org % artifact % version, Seq(tastyDep))) yield {
      val decoderJars = report
        .select(configurationFilter(Runtime.name), moduleFilter(), artifactFilter(extension = "jar", classifier = ""))
        .map(_.toURI.toURL)
        .toArray
      new URLClassLoader(decoderJars, null)
    }
  }

  private def fetchArtifactsOf(moduleID: ModuleID, dependencies: Seq[ModuleID]): Try[UpdateReport] = {
    val sha1 = Hash.toHex(Hash(moduleID.name))
    val dummyID = ModuleID("ch.epfl.scala.temp", "temp-module" + sha1, moduleID.revision)
      .withConfigurations(moduleID.configurations)
    val descriptor = dependencyRes.moduleDescriptor(dummyID, moduleID +: dependencies.toVector, None)

    dependencyRes.update(descriptor, updateConfig, warningConfig, logger) match {
      case Right(report) => Success(report)
      case Left(warning) => Failure(warning.resolveException)
    }
  }
}
