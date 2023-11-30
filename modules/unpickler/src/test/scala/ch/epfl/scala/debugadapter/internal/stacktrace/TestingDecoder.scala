package ch.epfl.scala.debugadapter.internal.stacktrace

import java.nio.file.Path
import ch.epfl.scala.debugadapter.ManagedEntry
import ch.epfl.scala.debugadapter.internal.binary
import ch.epfl.scala.debugadapter.internal.IO
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

class TestingDecoder(mainEntry: ManagedEntry, val decoder: BinaryDecoder):
  export decoder.*
  def name: String = mainEntry.name
  def allClasses: Seq[binary.ClassType] =
    val classNames = IO
      .withinJarFile(mainEntry.absolutePath) { fs =>
        val classMatcher = fs.getPathMatcher("glob:**.class")
        Files
          .walk(fs.getPath("/"): Path)
          .filter(classMatcher.matches)
          .iterator
          .asScala
          .map(_.toString.stripPrefix("/").stripSuffix(".class").replace('/', '.'))
          .filterNot(_.endsWith("module-info"))
          .toSeq
      }
      .get
    val classes = classNames.map(decoder.classLoader.loadClass)
    classes
