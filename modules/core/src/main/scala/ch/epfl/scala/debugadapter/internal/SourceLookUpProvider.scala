package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.{ClassEntry, Logger, Module}
import ch.epfl.scala.debugadapter.internal.scalasig.ScalaSig
import com.microsoft.java.debug.core.adapter.ISourceLookUpProvider
import scala.collection.mutable.{Map => MutableMap}

import java.net.URI
import scala.collection.parallel.immutable.ParVector
import ch.epfl.scala.debugadapter.internal.evaluator.NameTransformer

private[debugadapter] final class SourceLookUpProvider(
    private[internal] var classPathEntries: Seq[ClassEntryLookUp],
    private var sourceUriToClassPathEntry: Map[URI, ClassEntryLookUp],
    private var fqcnToClassPathEntry: Map[String, ClassEntryLookUp],
    logger: Logger
) extends ISourceLookUpProvider {

  val classSearch: MutableMap[String, Set[String]] = {
    val classesMap = MutableMap.empty[String, Set[String]].withDefaultValue(Set())
    for {
      entry <- classPathEntries
      e <- entry.fullyQualifiedNames.filterNot(_.contains("$$anon$"))
    } {
      val clsName = SourceLookUpProvider.getScalaClassName(e)
      classesMap(clsName) += e
    }

    classesMap
  }

  override def supportsRealtimeBreakpointVerification(): Boolean = true

  override def getSourceFileURI(fqcn: String, path: String): String = {
    getSourceFile(fqcn).map(_.toString).orNull
  }

  override def getSourceContents(uri: String): String = {
    val sourceUri = URI.create(uri)
    sourceUriToClassPathEntry
      .get(sourceUri)
      .flatMap(_.getSourceContent(sourceUri))
      .orNull
  }

  override def getFullyQualifiedName(
      uriRepr: String,
      lines: Array[Int],
      columns: Array[Int]
  ): Array[String] = {
    val uri = URI.create(uriRepr)
    uri.getScheme match {
      case "dap-fqcn" =>
        val resolvedName = uri.getSchemeSpecificPart
        lines.map(_ => resolvedName)
      case _ =>
        sourceUriToClassPathEntry.get(uri) match {
          case None => lines.map(_ => null)
          case Some(entry) =>
            lines.map(line => entry.getFullyQualifiedClassName(uri, line).orNull)
        }
    }
  }

  def getClassFile(fqcn: String): Option[ClassFile] =
    fqcnToClassPathEntry.get(fqcn).flatMap(_.getClassFile(fqcn))

  def getClassEntry(fqcn: String): Option[ClassEntry] =
    fqcnToClassPathEntry.get(fqcn).map(_.entry)

  def getSourceContentFromClassName(fqcn: String): Option[String] =
    fqcnToClassPathEntry.get(fqcn).flatMap(_.getSourceContentFromClassName(fqcn))

  def containsClass(fqcn: String): Boolean =
    fqcnToClassPathEntry.contains(fqcn)

  private[internal] def allClassNames: Iterable[String] =
    classPathEntries.flatMap(_.fullyQualifiedNames)
  private[internal] def allOrphanClasses: Iterable[ClassFile] =
    classPathEntries.flatMap(_.orphanClassFiles)
  private[internal] def classesByName(name: String): Set[String] =
    classSearch.get(name).getOrElse(Set.empty)

  private[internal] def getScalaSig(fqcn: String): Option[ScalaSig] = {
    for {
      classPathEntry <- fqcnToClassPathEntry.get(fqcn)
      scalaSig <- classPathEntry.getScalaSig(fqcn)
    } yield scalaSig
  }

  private def getSourceFile(className: String): Option[URI] = {
    fqcnToClassPathEntry
      .get(className)
      .flatMap(_.getSourceFile(className))
  }

  def reload(classesToReplace: Seq[String]): Unit = {
    classPathEntries = classPathEntries.map { lookUp =>
      lookUp.entry match {
        case m: Module => ClassEntryLookUp(m, logger)
        case _ => lookUp
      }
    }

    sourceUriToClassPathEntry = classPathEntries.flatMap(lookup => lookup.sources.map(uri => (uri, lookup))).toMap
    fqcnToClassPathEntry =
      classPathEntries.flatMap(lookup => lookup.fullyQualifiedNames.map(fqcn => (fqcn, lookup))).toMap

    classesToReplace.foreach { cls =>
      val clsName = SourceLookUpProvider.getScalaClassName(cls)
      classSearch(clsName) += cls
    }
  }
}

private[debugadapter] object SourceLookUpProvider {
  def empty(logger: Logger): SourceLookUpProvider =
    new SourceLookUpProvider(Seq.empty, Map.empty, Map.empty, logger)

  def getScalaClassName(className: String): String = {
    val lastDot = className.lastIndexOf('.') + 1
    val decoded = NameTransformer.decode(className.drop(lastDot))
    val lastDollar = decoded.stripSuffix("$").lastIndexOf('$') + 1
    decoded.drop(lastDollar)
  }

  def apply(entries: Seq[ClassEntry], logger: Logger): SourceLookUpProvider = {
    val parallelEntries = ParVector(entries*)
    val sourceLookUps =
      parallelEntries
        .flatMap(_.sourceEntries)
        .distinct
        .map(entry => entry -> SourceEntryLookUp(entry, logger))
        .toMap
    val allLookUps = parallelEntries
      .map(entry => ClassEntryLookUp(entry, entry.sourceEntries.flatMap(sourceLookUps.apply), logger))
      .seq
    val sourceUriToClassPathEntry = allLookUps
      .flatMap(lookup => lookup.sources.map(uri => (uri, lookup)))
      .toMap
    val fqcnToClassPathEntry = allLookUps
      .flatMap(lookup => lookup.fullyQualifiedNames.map(fqcn => (fqcn, lookup)))
      .toMap
    new SourceLookUpProvider(allLookUps, sourceUriToClassPathEntry, fqcnToClassPathEntry, logger)
  }
}
