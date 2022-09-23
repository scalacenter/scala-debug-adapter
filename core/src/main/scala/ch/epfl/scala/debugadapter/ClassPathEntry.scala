package ch.epfl.scala.debugadapter

import java.nio.file.Path
import java.net.URL

sealed trait ClassPathEntry extends ClassEntry {
  def absolutePath: Path

  override def classSystems: Seq[ClassSystem] = Seq(classSystem)

  def readBytes(classFile: String): Array[Byte] = classSystem.readBytes(classFile)

  private def classSystem: ClassSystem = if (isJar) ClassJar(absolutePath) else ClassDirectory(absolutePath)
  private def isJar: Boolean = absolutePath.toString.endsWith(".jar")
  def toURL: URL = absolutePath.toUri.toURL
}

final case class UnmanagedEntry(absolutePath: Path) extends ClassPathEntry {
  override def name: String = absolutePath.getFileName.toString
  override def sourceEntries: Seq[SourceEntry] = Seq.empty
}

sealed trait ManagedEntry extends ClassPathEntry {
  def scalaVersion: Option[ScalaVersion]
  def isScala2: Boolean = scalaVersion.exists(_.isScala2)
  def isScala3: Boolean = scalaVersion.exists(_.isScala3)
  def isJava: Boolean = scalaVersion.isEmpty
}

final case class Module(
    name: String,
    scalaVersion: Option[ScalaVersion],
    scalacOptions: Seq[String],
    absolutePath: Path,
    sourceEntries: Seq[SourceEntry]
) extends ManagedEntry

final case class Library(artifactId: String, version: String, absolutePath: Path, sourceEntries: Seq[SourceEntry])
    extends ManagedEntry {
  override def name: String = artifactId
  def scalaVersion: Option[ScalaVersion] = {
    if (artifactId == "scala-library") Some(ScalaVersion(version))
    else {
      artifactId
        .split('_')
        .lastOption
        .filter(bv => bv.startsWith("2.12") || bv.startsWith("2.13") || bv.startsWith("3"))
        .map(ScalaVersion.apply)
    }
  }
}
