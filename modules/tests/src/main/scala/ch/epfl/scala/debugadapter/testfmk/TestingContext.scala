package ch.epfl.scala.debugadapter.testfmk

import java.nio.file.Path
import ch.epfl.scala.debugadapter.ScalaVersion

trait TestingContext {
  def mainSource: Path
  def scalaVersion: ScalaVersion
}

object TestingContext {
  def apply(source: Path, sv: String): TestingContext =
    new TestingContext {
      val mainSource: Path = source
      val scalaVersion: ScalaVersion = ScalaVersion(sv)
    }
}
