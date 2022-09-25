package ch.epfl.scala.debugadapter

import coursier._
import java.io.File

object Coursier {
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
}
