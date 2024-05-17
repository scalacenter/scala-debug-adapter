package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter._
import ch.epfl.scala.debugadapter.internal.ScalaExtension._
import ch.epfl.scala.debugadapter.internal.evaluator.ExpressionCompiler

import scala.reflect.io.File

private[debugadapter] final class DebugTools(
    val expressionCompilers: Map[ClassEntry, ExpressionCompiler],
    val decoder: Option[ClassLoader],
    val sourceLookUp: SourceLookUpProvider
)

object DebugTools {

  def none(logger: Logger): DebugTools =
    new DebugTools(Map.empty, None, SourceLookUpProvider.empty(logger))

  /**
   * Resolve the expression compilers and the step filter of the debuggee,
   * Link all classes with source file in the SourceLookUpProvider
   *
   * TODO if scala3Entries is not empty we should use the Scala 3 step filter
   */
  def apply(debuggee: Debuggee, resolver: DebugToolsResolver, logger: Logger): DebugTools = {
    val allCompilers = TimeUtils.logTime(logger, "Loaded expression compiler") {
      loadExpressionCompiler(debuggee, resolver, logger)
    }

    val decoder =
      if (debuggee.scalaVersion.isScala3) {
        TimeUtils.logTime(logger, "Loaded step filter") {
          resolver
            .resolveDecoder(debuggee.scalaVersion)
            .warnFailure(logger, s"Cannot fetch decoder of Scala ${debuggee.scalaVersion}")
        }
      } else None

    val sourceLookUp = TimeUtils.logTime(logger, "Loaded all sources and classes") {
      val classEntries = debuggee.classEntries
      val distinctEntries = classEntries
        .groupBy(e => e.name)
        .map { case (name, group) =>
          if (group.size > 1) logger.warn(s"Found duplicate entry $name in debuggee ${debuggee.name}")
          group.head
        }
        .toSeq
      SourceLookUpProvider(distinctEntries, logger)
    }

    new DebugTools(allCompilers, decoder, sourceLookUp)
  }

  /* At most 2 expression compilers are resolved, one for Scala 2 and one for Scala 3
   * For both Scala 2 and Scala 3 we want to use the most recent known version:
   *   - the version of the main module
   *   - or the version of the scala-library/scala3-library in the classpath
   *   - or the latest known version of the scala-debug-adapter
   * That's because we want to be able to read the full classpath.
   * Examples:
   *  - if there are Scala 3.1 and 3.2 compiled jars in the classpath, we don't want to use the Scala 3.1 compiler
   *    because it cannot read the 3.2 jar.
   *  - similarly, only the latest Scala 2.13 version can unpickle the classes of the latest Scala 3 version
   *
   * The scalacOptions are adapted so that:
   *   - both compilers can unpickle Scala 2 AND Scala 3
   *   - both compilers does not fail on warnings
   */
  private def loadExpressionCompiler(
      debuggee: Debuggee,
      resolver: DebugToolsResolver,
      logger: Logger
  ): Map[ClassEntry, ExpressionCompiler] = {
    val scala2Entries = debuggee.managedEntries.filter(_.isScala2)
    val scala3Entries = debuggee.managedEntries.filter(_.isScala3)
    val scala2Version =
      if (debuggee.scalaVersion.isScala2) debuggee.scalaVersion
      else
        debuggee.libraries
          .find(_.artifactId == "scala-library")
          .flatMap(_.scalaVersion)
          .getOrElse(ScalaVersion.`2.13`)
    val scala3Version =
      if (debuggee.scalaVersion.isScala3) debuggee.scalaVersion
      else
        debuggee.libraries
          .find(_.artifactId.startsWith("scala3-library"))
          .map(lib => ScalaVersion(lib.version))
          .getOrElse(ScalaVersion.`3.1+`)

    def resolveCompilerClassLoader(scalaVersion: ScalaVersion): Option[ClassLoader] =
      resolver
        .resolveExpressionCompiler(scalaVersion)
        .warnFailure(logger, s"Cannot fetch expression compiler of Scala $scalaVersion")

    val scala3Loader = if (scala3Entries.isEmpty) None else resolveCompilerClassLoader(scala3Version)
    val scala2Loader = if (scala2Entries.isEmpty) None else resolveCompilerClassLoader(scala2Version)

    val defaultScala2Options =
      if (scala3Entries.nonEmpty) Seq("-Xsource:3", "-Ytasty-reader")
      else Seq("-Xsource:3")

    val classPath = debuggee.classPath.mkString(File.pathSeparator)

    def loadExpressionCompiler(entry: ManagedEntry): Option[(ClassEntry, ExpressionCompiler)] = {
      val optionsToAdd = if (entry.isScala2) defaultScala2Options else Seq.empty
      val scalacOptions = entry match {
        case module: Module => prepareOptions(module.scalacOptions, optionsToAdd)
        case lib: Library => optionsToAdd
      }
      for {
        classLoader <- if (entry.isScala2) scala2Loader else if (entry.isScala3) scala3Loader else None
        scalaVersion <- entry.scalaVersion
        compiler <- ExpressionCompiler(scalaVersion, scalacOptions, classPath, classLoader)
          .warnFailure(logger, s"Cannot load expression compiler of Scala $scalaVersion")
      } yield entry -> compiler
    }

    debuggee.managedEntries.flatMap(loadExpressionCompiler).toMap
  }

  private val optionsToRemove = Set("-Xfatal-warnings", "-Werror", "-indent", "-rewrite")

  private def prepareOptions(options: Seq[String], toAdd: Seq[String]) = {
    val withoutRemoved = options.filter(o => !optionsToRemove.contains(o))
    val withAdded = withoutRemoved ++ toAdd.filter(o => !options.contains(o))
    withAdded
  }
}
