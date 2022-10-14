package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.SourceEntry
import ch.epfl.scala.debugadapter.SourceJar
import ch.epfl.scala.debugadapter.SourceDirectory
import ch.epfl.scala.debugadapter.StandaloneSourceFile
import java.nio.file.FileSystems
import java.nio.file.FileSystem
import java.nio.file.Path
import java.nio.file.Files
import scala.jdk.CollectionConverters.*
import java.net.URI

private case class SourceFile(
    entry: SourceEntry,
    relativePath: String,
    uri: URI
) {
  def fileName: String = relativePath.split('/').last
  def folderPath: String = relativePath.stripSuffix(s"/$fileName")
}

private object SourceEntryLookUp {
  def getAllSourceFiles(entry: SourceEntry): Seq[SourceFile] = {
    entry match {
      case SourceJar(jar) =>
        IO.withinJarFile(jar) { fileSystem =>
          getAllSourceFiles(entry, fileSystem, fileSystem.getPath("/")).toVector
        }.getOrElse(Vector.empty)
      case SourceDirectory(directory) =>
        getAllSourceFiles(entry, FileSystems.getDefault, directory).toSeq
      case StandaloneSourceFile(absolutePath, relativePath) =>
        Seq(SourceFile(entry, relativePath, absolutePath.toUri))
    }
  }

  private def getAllSourceFiles(
      entry: SourceEntry,
      fileSystem: FileSystem,
      root: Path
  ): Iterator[SourceFile] = {
    if (Files.exists(root)) {
      val sourceMatcher = fileSystem.getPathMatcher("glob:**.{scala,java}")
      Files
        .walk(root: Path)
        .filter(sourceMatcher.matches)
        .iterator
        .asScala
        .map { path =>
          val relativePath = root.relativize(path).toString.replace('\\', '/')
          SourceFile(entry, relativePath, path.toUri)
        }
    } else Iterator.empty
  }
}
