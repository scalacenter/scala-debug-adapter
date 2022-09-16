package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.internal.IO

import java.nio.file.FileSystem
import java.nio.file.FileSystems
import java.nio.file.Path
import java.nio.file.Files
import java.net.URI
import scala.collection.mutable
import scala.util.control.NonFatal
import java.util.Collections

sealed trait ClassSystem {
  def within[T](f: (FileSystem, Path) => T): Option[T]
  def readBytes(path: String): Array[Byte] =
    within { (_, root) =>
      Files.readAllBytes(root.resolve(path))
    }.get
}

final case class ClassJar(absolutePath: Path) extends ClassSystem {
  def within[T](f: (FileSystem, Path) => T): Option[T] =
    IO.withinJarFile(absolutePath)(fs => f(fs, fs.getPath("/")))
}

final case class ClassDirectory(absolutePath: Path) extends ClassSystem {
  def within[T](f: (FileSystem, Path) => T): Option[T] =
    Some(f(FileSystems.getDefault, absolutePath))
}

final case class JavaRuntimeSystem(classLoader: ClassLoader, javaHome: Path) extends ClassSystem {
  def fileSystem: FileSystem =
    JavaRuntimeSystem.getFileSystem(classLoader, javaHome)

  def within[T](f: (FileSystem, Path) => T): Option[T] = {
    try {
      Some(f(fileSystem, fileSystem.getPath("/modules")))
    } catch {
      case NonFatal(_) => None
    }
  }
}

object JavaRuntimeSystem {
  private val fileSystems: mutable.Map[Path, FileSystem] = mutable.Map()

  def getFileSystem(classLoader: ClassLoader, javaHome: Path): FileSystem = {
    val properties = Collections.singletonMap("java.home", javaHome.toString)
    fileSystems.getOrElseUpdate(
      javaHome,
      FileSystems.newFileSystem(URI.create("jrt:/"), properties, classLoader)
    )
  }
}
