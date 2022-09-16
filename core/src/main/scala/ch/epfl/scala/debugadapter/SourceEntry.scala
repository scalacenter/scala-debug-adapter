package ch.epfl.scala.debugadapter

import java.nio.file.Path

sealed trait SourceEntry
final case class SourceDirectory(directory: Path) extends SourceEntry
final case class SourceJar(jar: Path) extends SourceEntry
final case class StandaloneSourceFile(absolutePath: Path, relativePath: String) extends SourceEntry
