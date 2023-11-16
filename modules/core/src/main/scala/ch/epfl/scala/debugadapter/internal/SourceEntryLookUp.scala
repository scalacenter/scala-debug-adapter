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
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.ScalaExtension.*
import scala.util.control.NonFatal
import scala.util.Properties

private case class SourceFile(
    entry: SourceEntry,
    relativePath: String,
    uri: URI
) {
  def fileName: String = relativePath.split('/').last
  def folderPath: String = relativePath.stripSuffix(s"/$fileName")
}

private case class SourceEntryLookUp(
    entry: SourceEntry,
    sourceFiles: Seq[SourceFile],
    fileSystem: FileSystem,
    root: Path
) {
  def close(): Unit =
    try
      entry match {
        case SourceJar(jar) => fileSystem.close()
        case SourceDirectory(directory) => ()
        case StandaloneSourceFile(absolutePath, relativePath) => ()
      }
    catch {
      case NonFatal(_) => ()
    }
}

private object SourceEntryLookUp {
  def apply(entry: SourceEntry, logger: Logger): Option[SourceEntryLookUp] = {
    entry match {
      case SourceJar(jar) =>
        IO.getJarFileSystem(jar)
          .map { fs =>
            val root = fs.getPath("/")
            val sourceFiles = getAllSourceFiles(entry, fs, root).toVector
            SourceEntryLookUp(entry, sourceFiles, fs, root)
          }
          .warnFailure(logger, s"Cannot list the source files in ${entry.name}")
      case SourceDirectory(directory) =>
        val fs = FileSystems.getDefault
        val sourceFiles = getAllSourceFiles(entry, fs, directory).toVector
        Some(SourceEntryLookUp(entry, sourceFiles, fs, directory))
      case StandaloneSourceFile(absolutePath, relativePath) =>
        val fs = FileSystems.getDefault
        val sourceFile = SourceFile(entry, relativePath, absolutePath.toUri)
        val root = fs.getPath(sourceFile.folderPath)
        Some(SourceEntryLookUp(entry, Seq(sourceFile), FileSystems.getDefault, root))
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
