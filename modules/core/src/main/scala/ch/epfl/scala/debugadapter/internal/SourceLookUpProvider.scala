package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.ClassEntry
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.scalasig.ScalaSig
import com.microsoft.java.debug.core.adapter.ISourceLookUpProvider

import java.net.URI
import scala.collection.parallel.immutable.ParVector
import ch.epfl.scala.debugadapter.internal.evaluator.NameTransformer

private[debugadapter] final class SourceLookUpProvider(
    private[internal] val classPathEntries: Seq[ClassEntryLookUp],
    sourceUriToClassPathEntry: Map[URI, ClassEntryLookUp],
    fqcnToClassPathEntry: Map[String, ClassEntryLookUp]
) extends ISourceLookUpProvider {

  val classSearch =
    classPathEntries
      .flatMap { _.fullyQualifiedNames.filterNot { _.contains("$$anon$") } }
      .groupBy { SourceLookUpProvider.getScalaClassName }

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
  private[internal] def classesByName(name: String): Seq[String] =
    classSearch.find(_._1 == name).map(_._2).getOrElse(Seq.empty)

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
}

private[debugadapter] object SourceLookUpProvider {
  def empty: SourceLookUpProvider =
    new SourceLookUpProvider(Seq.empty, Map.empty, Map.empty)

  def getScalaClassName(className: String): String = {
    val lastDot = className.lastIndexOf('.') + 1
    val decoded = NameTransformer.decode { className.drop(lastDot) }
    val lastDollar = decoded.stripSuffix("$").lastIndexOf('$') + 1
    decoded.drop { lastDollar }
  }

  def apply(entries: Seq[ClassEntry], logger: Logger): SourceLookUpProvider = {
    val parrallelEntries = ParVector(entries*)
    val sourceFilesByEntry = parrallelEntries
      .flatMap(_.sourceEntries)
      .distinct
      .map(entry => entry -> SourceEntryLookUp.getAllSourceFiles(entry, logger))
      .toMap
    val allLookUps = parrallelEntries
      .map(entry => ClassEntryLookUp(entry, entry.sourceEntries.flatMap(sourceFilesByEntry.apply), logger))
      .seq
    val sourceUriToClassPathEntry = allLookUps
      .flatMap(lookup => lookup.sources.map(uri => (uri, lookup)))
      .toMap
    val fqcnToClassPathEntry = allLookUps
      .flatMap(lookup => lookup.fullyQualifiedNames.map(fqcn => (fqcn, lookup)))
      .toMap
    new SourceLookUpProvider(
      allLookUps,
      sourceUriToClassPathEntry,
      fqcnToClassPathEntry
    )
  }
}
