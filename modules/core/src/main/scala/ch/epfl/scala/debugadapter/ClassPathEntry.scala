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
  override def name: String = absolutePath.toString
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

  private def versionSuffix = artifactId.split('_').lastOption

  override def isScala2: Boolean =
    scalaVersion.exists(_.isScala2) || versionSuffix.exists(_.startsWith("2."))
  override def isScala3: Boolean =
    scalaVersion.exists(_.isScala3) || versionSuffix.exists(_.startsWith("3"))
  override def isJava: Boolean =
    scalaVersion.isEmpty && !versionSuffix.exists(suffix => suffix.startsWith("2.") || suffix.startsWith("3."))

  def scalaVersion: Option[ScalaVersion] = {
    if (
      artifactId == "scala-library" || artifactId.startsWith("scala3-library_3") ||
      artifactId.startsWith("scala-compiler") || artifactId.startsWith("scala-compiler_3")
    )
      Some(ScalaVersion(version))
    else None
  }
}
