package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.{ClassPathEntry, Logger}
import com.microsoft.java.debug.core.adapter.ISourceLookUpProvider

import java.net.URI

private[debugadapter] final class SourceLookUpProvider(
  private[internal] val classPathEntries: Seq[ClassPathEntryLookUp],
  sourceUriToClassPathEntry: Map[URI, ClassPathEntryLookUp],
  fqcnToClassPathEntry: Map[String, ClassPathEntryLookUp],
  logger: Logger
) extends ISourceLookUpProvider {
  override def supportsRealtimeBreakpointVerification(): Boolean = true

  override def getSourceFileURI(fqcn: String, path: String): String = {
    fqcnToClassPathEntry.get(fqcn)
      .flatMap(_.getSourceFile(fqcn))
      .map(_.toString)
      .orNull
  }
  
  override def getSourceContents(uri: String): String = {
    val sourceUri = URI.create(uri)
    sourceUriToClassPathEntry.get(sourceUri)
      .flatMap(_.getSourceContent(sourceUri))
      .orNull
  }

  override def getFullyQualifiedName(uriRepr: String, lines: Array[Int], columns: Array[Int]): Array[String] = {
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

  private[internal] def allClassNames: Iterable[String] = classPathEntries.flatMap(_.fullyQualifiedNames)
  private[internal] def allOrphanClasses: Iterable[ClassFile] = classPathEntries.flatMap(_.orphanClassFiles)  
}

private[debugadapter] object SourceLookUpProvider {
  def apply(classPath: Seq[ClassPathEntry], logger: Logger): SourceLookUpProvider = {
    val classPathEntries = classPath.par.map(ClassPathEntryLookUp.apply).seq
    val sourceUriToClassPathEntry = classPathEntries
      .flatMap(lookup => lookup.sources.map(uri => (uri, lookup)))
      .toMap
    val fqcnToClassPathEntry = classPathEntries
      .flatMap(lookup => lookup.fullyQualifiedNames.map(fqcn => (fqcn, lookup)))
      .toMap
    new SourceLookUpProvider(classPathEntries, sourceUriToClassPathEntry, fqcnToClassPathEntry, logger)
  }
}
