package ch.epfl.scala.debugadapter.testfmk

import ch.epfl.scala.debugadapter.Library
import java.net.URLClassLoader
import java.nio.file.Path
import ch.epfl.scala.debugadapter.ClassPathEntry
import java.io.File

sealed abstract class ScalaInstance(
    val libraryJars: Seq[Library],
    compilerJars: Seq[Library],
    expressionCompilerJar: Library,
    decoderJars: Seq[Library]
) {
  val libraryClassLoader = new URLClassLoader(libraryJars.map(_.toURL).toArray, null)
  val compilerClassLoader = new URLClassLoader(compilerJars.map(_.toURL).toArray, libraryClassLoader)
  val expressionCompilerClassLoader = new URLClassLoader(Array(expressionCompilerJar.toURL), compilerClassLoader)
  val decoderClassLoader = new URLClassLoader(decoderJars.map(_.toURL).toArray, null)

  def compile(
      classDir: Path,
      classPath: Seq[ClassPathEntry],
      scalacOptions: Seq[String],
      sourceFiles: Seq[Path]
  ): Unit = {
    val args = Array(
      "-d",
      classDir.toString,
      "-classpath",
      classPath.map(_.absolutePath).mkString(File.pathSeparator)
    ) ++
      scalacOptions ++
      sourceFiles.map(_.toString)
    compileInternal(args)
  }

  protected def compileInternal(args: Array[String]): Unit
}

final class Scala2Instance(
    libraryJars: Seq[Library],
    compilerJars: Seq[Library],
    expressionCompilerJar: Library
) extends ScalaInstance(libraryJars, compilerJars, expressionCompilerJar, Seq.empty) {
  override protected def compileInternal(args: Array[String]): Unit = {
    val main = compilerClassLoader.loadClass("scala.tools.nsc.Main")
    val process = main.getMethod("process", classOf[Array[String]])
    val success = process.invoke(null, args).asInstanceOf[Boolean]
    if (!success) throw new Exception("compilation failed")
  }
}

final class Scala3Instance(
    libraryJars: Seq[Library],
    compilerJars: Seq[Library],
    expressionCompilerJar: Library,
    stepFilterJars: Seq[Library]
) extends ScalaInstance(libraryJars, compilerJars, expressionCompilerJar, stepFilterJars) {
  override protected def compileInternal(args: Array[String]): Unit = {
    val main = compilerClassLoader.loadClass("dotty.tools.dotc.Main")
    val process = main.getMethod("process", classOf[Array[String]])
    val classOfReporter =
      compilerClassLoader.loadClass("dotty.tools.dotc.reporting.Reporter")
    val hasErrors = classOfReporter.getMethod("hasErrors")
    val reporter = process.invoke(null, args)
    val success = !(hasErrors.invoke(reporter).asInstanceOf[Boolean])
    if (!success) throw new Exception("compilation failed")
  }
}
