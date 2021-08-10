package ch.epfl.scala.debugadapter

import java.nio.file.Path

case class ClassPathEntry(absolutePath: Path, sourceEntries: Seq[SourceEntry]) {
  def name: String = absolutePath.getFileName.toString.stripSuffix(".jar")
  def isJar: Boolean = absolutePath.toString.endsWith(".jar")
}
