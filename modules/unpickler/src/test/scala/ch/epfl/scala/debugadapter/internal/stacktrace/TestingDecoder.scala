package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.Java8
import ch.epfl.scala.debugadapter.Java9OrAbove
import ch.epfl.scala.debugadapter.JavaRuntime
import ch.epfl.scala.debugadapter.ManagedEntry
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.internal.IO
import ch.epfl.scala.debugadapter.internal.binary
import ch.epfl.scala.debugadapter.internal.binary.BinaryClassLoader
import ch.epfl.scala.debugadapter.internal.javareflect.JavaReflectLoader
import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee

import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters.*
import scala.util.Properties

object TestingDecoder:
  def javaRuntime = JavaRuntime(Properties.jdkHome).get

  def apply(source: String, scalaVersion: ScalaVersion)(using ThrowOrWarn): TestingDecoder =
    val debuggee = TestingDebuggee.mainClass(source, "decoder", scalaVersion)
    TestingDecoder(debuggee.mainModule, debuggee.managedEntries)

  def apply(source: String, scalaVersion: ScalaVersion, dependencies: Seq[ManagedEntry])(using
      ThrowOrWarn
  ): TestingDecoder =
    val debuggee = TestingDebuggee.mainClass(source, "decoder", scalaVersion, Seq.empty, dependencies)
    TestingDecoder(debuggee.mainModule, debuggee.managedEntries)

  def apply(mainModule: ManagedEntry, classEntries: Seq[ManagedEntry])(using ThrowOrWarn): TestingDecoder =
    val classPath = classEntries.map(_.absolutePath)
    val javaRuntimeJars = javaRuntime match
      case Java8(_, classJars, _) => classJars
      case java9OrAbove: Java9OrAbove =>
        java9OrAbove.classSystems.flatMap { s =>
          Files.list(s.fileSystem.getPath("/modules")).iterator.asScala.toSeq
        }
    val decoder = BinaryDecoder(classPath ++ javaRuntimeJars)
    val classLoader = JavaReflectLoader(classPath)
    new TestingDecoder(mainModule, classLoader, decoder)

class TestingDecoder(mainEntry: ManagedEntry, val classLoader: BinaryClassLoader, val decoder: BinaryDecoder):
  export decoder.*

  def decode(cls: String): DecodedClass =
    val binaryClass = classLoader.loadClass(cls)
    decode(binaryClass)
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
    val classes = classNames.map(classLoader.loadClass)
    classes
