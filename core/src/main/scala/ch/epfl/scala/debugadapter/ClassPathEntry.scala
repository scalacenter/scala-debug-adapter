package ch.epfl.scala.debugadapter

import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.Paths

case class ClassPathEntry(absolutePath: Path, sourceEntries: Seq[SourceEntry]) extends ClassEntry {
  override def classSystems: Seq[ClassSystem] = {
    if (isJar) Seq(ClassJar(absolutePath))
    else Seq(ClassDirectory(absolutePath))
  }
  private def name: String = absolutePath.getFileName.toString.stripSuffix(".jar")
  private def isJar: Boolean = absolutePath.toString.endsWith(".jar")
}
