package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.internal.IO

import java.nio.file.FileSystem
import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.Path

sealed trait ClassSystem {
  def within[T](f: (FileSystem, Path) => T): T
}

final case class ClassJar(absolutePath: Path) extends ClassSystem {
  def within[T](f: (FileSystem, Path) => T): T =
    IO.withinJarFile(absolutePath)(fs => f(fs, fs.getPath("/")))
}

final case class ClassDirectory(absolutePath: Path) extends ClassSystem {
  def within[T](f: (FileSystem, Path) => T): T =
    f(FileSystems.getDefault, absolutePath)
}

final case class JavaRuntimeSystem(classLoader: ClassLoader, javaHome: Path)
    extends ClassSystem {
  def within[T](f: (FileSystem, Path) => T): T = {
    IO.withinJavaRuntimeFileSystem(classLoader, javaHome)(fs =>
      f(fs, fs.getPath("/modules"))
    )
  }
}
