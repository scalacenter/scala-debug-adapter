package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.ClassEntry
import com.microsoft.java.debug.core.adapter.ISourceLookUpProvider

import java.net.URI

private[debugadapter] final class SourceLookUpProvider(
    private[internal] val classPathEntries: Seq[ClassEntryLookUp],
    sourceUriToClassPathEntry: Map[URI, ClassEntryLookUp],
    fqcnToClassPathEntry: Map[String, ClassEntryLookUp]
) extends ISourceLookUpProvider {
  override def supportsRealtimeBreakpointVerification(): Boolean = true

  override def getSourceFileURI(fqcn: String, path: String): String = {
    fqcnToClassPathEntry
      .get(fqcn)
      .flatMap(_.getSourceFile(fqcn))
      .map(_.toString)
      .orNull
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
            lines.map(line =>
              entry.getFullyQualifiedClassName(uri, line).orNull
            )
        }
    }
  }

  private[internal] def allClassNames: Iterable[String] =
    classPathEntries.flatMap(_.fullyQualifiedNames)
  private[internal] def allOrphanClasses: Iterable[ClassFile] =
    classPathEntries.flatMap(_.orphanClassFiles)

  private[internal] def getClassFile(className: String): Option[String] = {
     val classEntryLookup = fqcnToClassPathEntry.get(className).get
     val uri = classEntryLookup.getSourceFile(className).get
     
     //classEntryLookup.getClassFile(uri) Doe we want classfiles?

     classEntryLookup.getSourceContent(uri)
     
  }
}

private[debugadapter] object SourceLookUpProvider {
  def apply(entries: Seq[ClassEntry]): SourceLookUpProvider = {
    val sourceEntries = entries.flatMap(_.sourceEntries).distinct
    val sourceFilesByEntry = sourceEntries.par
      .map(entry => entry -> SourceEntryLookUp.getAllSourceFiles(entry))
      .toMap
    val allLookUps = entries.par.map { entry =>
      ClassEntryLookUp(
        entry.classSystems,
        entry.sourceEntries.flatMap(sourceFilesByEntry.apply)
      )
    }.seq
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
