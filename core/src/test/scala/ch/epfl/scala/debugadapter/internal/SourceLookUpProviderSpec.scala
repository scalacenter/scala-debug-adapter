package ch.epfl.scala.debugadapter.internal

import utest._
import coursier._
import java.io.File
import ch.epfl.scala.debugadapter.ClassPathEntry
import ch.epfl.scala.debugadapter.SourceJar
import ch.epfl.scala.debugadapter.NoopLogger

object SourceLookUpProviderSpec extends TestSuite {
  def tests: Tests = Tests {
    "fix https://github.com/scalameta/metals/issues/3477#issuecomment-1013458270" - {
      val artifacts = coursier
        .Fetch()
        .addDependencies(dep"org.openjfx:javafx-controls:17.0.1")
        .addClassifiers(
          Classifier.sources,
          Classifier("win"),
          Classifier("linux"),
          Classifier("mac")
        )
        .withMainArtifacts()
        .run()

      val classPath = artifacts
        .groupBy(getArtifactId)
        .values
        .flatMap { group =>
          val sourcesJar = group.find(_.getName.endsWith("-sources.jar")).get
          val winJar = group.find(_.getName.endsWith("-win.jar")).get
          val linuxJar = group.find(_.getName.endsWith("-linux.jar")).get
          val macJar = group.find(_.getName.endsWith("-mac.jar")).get
          val mainJar = group
            .find(file => !Set(sourcesJar, winJar, linuxJar, macJar).contains(file))
            .get
          val sourceEntries = Seq(SourceJar(sourcesJar.toPath))
          Seq(winJar, linuxJar, macJar, mainJar)
            .map(_.toPath)
            .map(ClassPathEntry(_, sourceEntries))
        }
        .toSeq

      for (_ <- 0 until 10) SourceLookUpProvider(classPath, NoopLogger)
    }
  }

  private def getArtifactId(file: File): String = {
    file.getName
      .stripSuffix(".jar")
      .stripSuffix("-sources")
      .stripSuffix("-win")
      .stripSuffix("-linux")
      .stripSuffix("-mac")
  }
}
