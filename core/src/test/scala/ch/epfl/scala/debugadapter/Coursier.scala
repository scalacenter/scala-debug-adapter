package ch.epfl.scala.debugadapter

import coursier._

import java.io.File

object Coursier {
  def fetchOnly(org: String, name: String, version: String): ClassPathEntry = {
    val dependency = Dependency(Module(Organization(org), ModuleName(name)), version)
    fetch(dependency)
      .find(_.absolutePath.getFileName.toString == s"$name-$version.jar")
      .get
  }

  def fetch(dependency: Dependency): Seq[ClassPathEntry] = fetch(Seq(dependency))

  def fetch(dependencies: Seq[Dependency]): Seq[ClassPathEntry] = {
    coursier.Fetch()
      .addDependencies(dependencies:_*)
      .addClassifiers(Classifier.sources)
      .withMainArtifacts()
      .run()
      .groupBy(getArtifactId)
      .flatMap { case (_, jars) => 
        for {
          sourceJar <- jars.find(_.getName.endsWith("-sources.jar"))
          classJar <- jars.find(_ != sourceJar)
        } yield ClassPathEntry(classJar.toPath, Seq(SourceJar(sourceJar.toPath)))
      }
      .toSeq
  }

  private def getArtifactId(file: File): String = {
    file.getName.stripSuffix(".jar").stripSuffix("-sources")
  } 
}
