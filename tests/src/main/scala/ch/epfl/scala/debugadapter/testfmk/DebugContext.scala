package ch.epfl.scala.debugadapter.testfmk

import java.nio.file.Path
import ch.epfl.scala.debugadapter.ScalaVersion

trait DebugContext {
  def mainSource: Path
  def scalaVersion: ScalaVersion
}

object DebugContext {
  def apply(source: Path, sv: String): DebugContext =
    new DebugContext {
      def mainSource: Path = source
      def scalaVersion: ScalaVersion = ScalaVersion(sv)
    }
}
