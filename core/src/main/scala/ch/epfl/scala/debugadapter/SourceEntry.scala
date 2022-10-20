package ch.epfl.scala.debugadapter

import java.nio.file.Path

sealed trait SourceEntry {
  def name: String
}
final case class SourceDirectory(directory: Path) extends SourceEntry {
  def name: String = directory.toString
}
final case class SourceJar(jar: Path) extends SourceEntry {
  def name: String = jar.toString
}
final case class StandaloneSourceFile(absolutePath: Path, relativePath: String) extends SourceEntry {
  def name: String = relativePath.toString
}
