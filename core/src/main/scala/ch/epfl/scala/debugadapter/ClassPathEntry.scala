package ch.epfl.scala.debugadapter

import java.nio.file.Path
import java.net.URL
import java.nio.file.Files

case class ClassPathEntry(absolutePath: Path, sourceEntries: Seq[SourceEntry])
    extends ClassEntry {
  override def classSystems: Seq[ClassSystem] = {
    if (isJar) Seq(ClassJar(absolutePath))
    else Seq(ClassDirectory(absolutePath))
  }
  def toURL: URL = absolutePath.toUri.toURL
  def name: String =
    absolutePath.getFileName.toString.stripSuffix(".jar")
  private def isJar: Boolean = absolutePath.toString.endsWith(".jar")

  def   readBytes(classFile: String): Array[Byte] = {
    classSystems.head.within((_, path) => Files.readAllBytes(path.resolve(classFile))).get
  }
}
