package ch.epfl.scala.debugadapter

import java.nio.file.Path
import java.net.URL

case class ClassPathEntry(absolutePath: Path, sourceEntries: Seq[SourceEntry])
    extends ClassEntry {
  def classSystem: ClassSystem =
    if (isJar) ClassJar(absolutePath)
    else ClassDirectory(absolutePath)
  override def classSystems: Seq[ClassSystem] = Seq(classSystem)
  def toURL: URL = absolutePath.toUri.toURL
  def name: String =
    absolutePath.getFileName.toString.stripSuffix(".jar")
  private def isJar: Boolean = absolutePath.toString.endsWith(".jar")

  def readBytes(classFile: String): Array[Byte] =
    classSystem.readBytes(classFile)
}
