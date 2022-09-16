package ch.epfl.scala.debugadapter.internal

import java.net.URI
import java.nio.file.FileSystem
import java.nio.file.FileSystems
import java.nio.file.Path
import java.util
import scala.util.control.NonFatal

private[debugadapter] object IO {
  def withinJarFile[T](absolutePath: Path)(f: FileSystem => T): Option[T] = {
    val uri = URI.create(s"jar:${absolutePath.toUri}")
    val fileSystem =
      FileSystems.newFileSystem(uri, new util.HashMap[String, Any])
    try Some(f(fileSystem))
    catch {
      case NonFatal(e) => None
    } finally fileSystem.close()
  }
}
