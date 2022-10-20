package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.internal.IO

import java.nio.file.FileSystem
import java.nio.file.FileSystems
import java.nio.file.Path
import java.nio.file.Files
import java.net.URI
import scala.collection.mutable
import java.util.Collections
import scala.util.Try

sealed trait ClassSystem {
  def name: String
  def within[T](f: (FileSystem, Path) => T): Try[T]
  def readBytes(path: String): Array[Byte] =
    within { (_, root) =>
      Files.readAllBytes(root.resolve(path))
    }.get
}

final case class ClassJar(absolutePath: Path) extends ClassSystem {
  def name: String = absolutePath.toString
  def within[T](f: (FileSystem, Path) => T): Try[T] =
    IO.withinJarFile(absolutePath)(fs => f(fs, fs.getPath("/")))
}

final case class ClassDirectory(absolutePath: Path) extends ClassSystem {
  def name: String = absolutePath.toString
  def within[T](f: (FileSystem, Path) => T): Try[T] = Try {
    f(FileSystems.getDefault, absolutePath)
  }
}

final case class JavaRuntimeSystem(classLoader: ClassLoader, javaHome: Path) extends ClassSystem {
  def name: String = javaHome.toString
  def fileSystem: FileSystem =
    JavaRuntimeSystem.getFileSystem(classLoader, javaHome)

  def within[T](f: (FileSystem, Path) => T): Try[T] = Try {
    f(fileSystem, fileSystem.getPath("/modules"))
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
