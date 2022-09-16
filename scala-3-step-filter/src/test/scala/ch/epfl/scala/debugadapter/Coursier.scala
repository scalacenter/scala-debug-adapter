package ch.epfl.scala.debugadapter

import coursier._

import java.io.File
import java.nio.file.Path

object Coursier {
  def fetchOnly(org: String, name: String, version: String): Path = {
    fetch(org, name, version)
      .find(_.getFileName.toString == s"$name-$version.jar")
      .get
  }

  def fetch(org: String, name: String, version: String): Seq[Path] = {
    val dependency =
      Dependency(Module(Organization(org), ModuleName(name)), version)
    fetch(dependency)
  }

  def fetch(dependencies: Dependency*): Seq[Path] = {
    coursier
      .Fetch()
      .addDependencies(dependencies: _*)
      .addClassifiers(Classifier.sources)
      .withMainArtifacts()
      .run()
      .groupBy(getArtifactId)
      .flatMap { case (_, jars) =>
        for {
          sourceJar <- jars.find(_.getName.endsWith("-sources.jar"))
          classJar <- jars.find(_ != sourceJar)
        } yield classJar.toPath
      }
      .toSeq
  }

  private def getArtifactId(file: File): String = {
    file.getName.stripSuffix(".jar").stripSuffix("-sources")
  }
}
