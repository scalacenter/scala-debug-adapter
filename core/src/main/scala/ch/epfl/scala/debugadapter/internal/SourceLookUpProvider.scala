package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.{ClassPathEntry, Logger}
import com.microsoft.java.debug.core.adapter.ISourceLookUpProvider

import java.net.URI

private[debugadapter] final class SourceLookUpProvider( 
  sourceUriToClassPathEntry: Map[URI, ClassPathEntryLookUp],
  fqcnToClassPathEntry: Map[String, ClassPathEntryLookUp],
  logger: Logger
) extends ISourceLookUpProvider {
  override def supportsRealtimeBreakpointVerification(): Boolean = true

  override def getSourceFileURI(fqcn: String, path: String): String = {
    fqcnToClassPathEntry.get(fqcn) match {
      case None => null
      case Some(entry) => entry.getSourceFile(fqcn).map(_.toString).orNull
    }
  }
  
  override def getSourceContents(uri: String): String = {
    val sourceUri = URI.create(uri)
    sourceUriToClassPathEntry.get(sourceUri) match {
      case None => null
      case Some(entry) => entry.getSourceContent(sourceUri).orNull
    }
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
}

private[debugadapter] object SourceLookUpProvider {
  def apply(classPath: Seq[ClassPathEntry], logger: Logger): SourceLookUpProvider = {
    val lookups = classPath.par.map(ClassPathEntryLookUp.apply).seq
    val sourceUriToClassPathEntry = lookups
      .flatMap(lookup => lookup.sources.map(uri => (uri, lookup)))
      .toMap
    val fqcnToClassPathEntry = lookups
      .flatMap(lookup => lookup.fullyQualifiedNames.map(fqcn => (fqcn, lookup)))
      .toMap
    new SourceLookUpProvider(sourceUriToClassPathEntry, fqcnToClassPathEntry, logger)
  }
}
