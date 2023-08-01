package ch.epfl.scala.debugadapter.internal

import java.net.URI
import java.nio.file.FileSystem
import java.nio.file.FileSystems
import java.nio.file.Path
import java.util
import scala.util.Try
import scala.util.control.NonFatal
import scala.util.Failure
import scala.util.Success

private[debugadapter] object IO {
  def withinJarFile[T](absolutePath: Path)(f: FileSystem => T): Try[T] =
    getJarFileSystem(absolutePath).map { fs =>
      try f(fs)
      finally fs.close()
    }

  def getJarFileSystem(absolutePath: Path): Try[FileSystem] = try {
    val uri = URI.create(s"jar:${absolutePath.toUri}")
    val fileSystem = FileSystems.newFileSystem(uri, new util.HashMap[String, Any])
    Success(fileSystem)
  } catch {
    case NonFatal(exception) => Failure(exception)
    case zipError: util.zip.ZipError => Failure(zipError)
  }
}
