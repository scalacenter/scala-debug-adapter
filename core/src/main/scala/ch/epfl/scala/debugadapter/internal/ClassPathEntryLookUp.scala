package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter._
import org.objectweb.asm._

import java.net.URI
import java.nio.file._
import java.util
import scala.collection.JavaConverters._
import scala.collection.mutable
import ClassPathEntryLookUp.{readSourceContent, withinJarFile}

import scala.util.matching.Regex

private case class SourceLine(uri: URI, lineNumber: Int)

private case class ClassFile(fullyQualifiedName: String, sourceName: Option[String], relativePath: String) {
  def className: String = fullyQualifiedName.split('.').last
  def fullPackage: String = fullyQualifiedName.stripSuffix(className).stripSuffix(".")
}

private case class SourceFile(entry: SourceEntry, relativePath: String, uri: URI) {
  def extension: String = relativePath.split('.').last
  def baseName: String = name.stripSuffix(extension)
  def name: String = relativePath.split('/').last
  def folderAsPackage: String = relativePath.stripSuffix(name).stripSuffix("/").replace('/', '.')
}

private class ClassPathEntryLookUp(
  val entry: ClassPathEntry,
  sourceUriToSourceFile: Map[URI, SourceFile],
  sourceUriToClassFiles: Map[URI, Seq[ClassFile]],
  classNameToSourceFile: Map[String, SourceFile],
  missingSourceFileClassFiles: Seq[ClassFile],
  private[internal] val orphanClassFiles: Seq[ClassFile]
) {
  private val cachedSourceLines = mutable.Map[SourceLine, Seq[ClassFile]]()

  def sources: Iterable[URI] = sourceUriToSourceFile.keys
  def fullyQualifiedNames: Iterable[String] = {
    classNameToSourceFile.keys ++ 
      orphanClassFiles.map(_.fullyQualifiedName) ++ 
      missingSourceFileClassFiles.map(_.fullyQualifiedName)
  }

  def getFullyQualifiedClassName(sourceUri: URI, lineNumber: Int): Option[String] = {
    val line = SourceLine(sourceUri, lineNumber)
  
    if (!cachedSourceLines.contains(line)) {
      // read and cache line numbers from class files
      for (classFiles <- sourceUriToClassFiles.get(sourceUri)) {
        if (entry.isJar) withinJarFile(entry.absolutePath)(loadLineNumbers(_, classFiles, sourceUri))
        else loadLineNumbers(FileSystems.getDefault, classFiles, sourceUri)
      }
    }

    cachedSourceLines.get(line)
      .map { classFiles =>
        // The same breakpoint can stop in different classes
        // We choose the one with the smallest name
        classFiles.map(_.fullyQualifiedName).minBy(_.length)
      }
  }

  private def loadLineNumbers(fileSystem: FileSystem, classFiles: Seq[ClassFile], sourceUri: URI): Unit = {
    for (classFile <- classFiles) {
      val path = fileSystem.getPath(classFile.relativePath)
      val inputStream = Files.newInputStream(path)
      val reader = new ClassReader(inputStream)

      val lineNumbers = mutable.Buffer[Int]()
    
      val visitor = new ClassVisitor(Opcodes.ASM7) {
        override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
          new MethodVisitor(Opcodes.ASM7) {
            override def visitLineNumber(line: Int, start: Label): Unit = {
              lineNumbers.append(line)
            }
          }
        }
      }
      reader.accept(visitor, 0)
      
      for (n <- lineNumbers) {
        val line = SourceLine(sourceUri, n)
        cachedSourceLines.update(
          line,
          cachedSourceLines.getOrElse(line, Seq.empty) :+ classFile
        )
      }
    }
  }

  def getSourceContent(sourceUri: URI): Option[String] = {
    sourceUriToSourceFile.get(sourceUri).map(readSourceContent)
  }

  def getSourceFile(fqcn: String): Option[URI] = {
    classNameToSourceFile.get(fqcn).map(_.uri)
  }
}

private object ClassPathEntryLookUp {
  def empty(entry: ClassPathEntry): ClassPathEntryLookUp =
    new ClassPathEntryLookUp(entry, Map.empty, Map.empty, Map.empty, Seq.empty, Seq.empty)

  def apply(entry: ClassPathEntry): ClassPathEntryLookUp = {
    val sourceFiles = entry.sourceEntries.flatMap(getAllSourceFiles)
    if (sourceFiles.isEmpty) ClassPathEntryLookUp.empty(entry)
    else {
      val classFiles = readAllClassFiles(entry)
      val sourceUriToSourceFile = sourceFiles.map(f => (f.uri, f)).toMap
      val sourceNameToSourceFile = sourceFiles.groupBy(f => f.name)
      
      val classNameToSourceFile = mutable.Map[String, SourceFile]()
      val sourceUriToClassFiles = mutable.Map[URI, Seq[ClassFile]]()
      val orphanClassFiles = mutable.Buffer[ClassFile]()
      val missingSourceFileClassFiles = mutable.Buffer[ClassFile]()

      for (classFile <- classFiles) {
        def recordSourceFile(sourceFile: SourceFile): Unit = {
          classNameToSourceFile.put(classFile.fullyQualifiedName, sourceFile)
          sourceUriToClassFiles.update(
            sourceFile.uri,
            sourceUriToClassFiles.getOrElse(sourceFile.uri, Seq.empty) :+ classFile
          )
        }

        classFile.sourceName.flatMap(sourceNameToSourceFile.get).getOrElse(Seq.empty).toList match {
          case Nil =>
            // the source name is missing from the class file
            // or the source file is missing from the source entr
            missingSourceFileClassFiles.append(classFile)
          case sourceFile:: Nil =>
            // there is only one file with that name, it must be the right one
            // even if its relative path does not match the class package
            recordSourceFile(sourceFile)
          case manySourceFiles =>
            // there are several files with the same name
            // we find the one whose relative path matches the class package
            manySourceFiles.find(f => f.folderAsPackage == classFile.fullPackage) match {
              case Some(sourceFile) => recordSourceFile(sourceFile)
              case None =>
                // there is no source file with the correct relative path
                // so we try to find the right package declaration in each file
                // it would be very unfortunate that 2 sources file with the same name
                // declare the same package.
                manySourceFiles.filter(s => findPackage(s, classFile.fullPackage)) match {
                  case sourceFile :: Nil =>
                    recordSourceFile(sourceFile)
                  case _ =>
                    orphanClassFiles.append(classFile)
                }
            }
        }
      }

      new ClassPathEntryLookUp(
        entry,
        sourceUriToSourceFile,
        sourceUriToClassFiles.toMap,
        classNameToSourceFile.toMap,
        missingSourceFileClassFiles,
        orphanClassFiles
      )
    }
  }

  private def getAllSourceFiles(entry: SourceEntry): Seq[SourceFile] = {
    entry match {
      case SourceJar(jar) => 
        withinJarFile(jar) { fileSystem =>
          getAllSourceFiles(entry, fileSystem, fileSystem.getPath("/")).toVector
        }
      case SourceDirectory(directory) =>
        getAllSourceFiles(entry, FileSystems.getDefault, directory).toSeq
      case StandaloneSourceFile(absolutePath, relativePath) =>
        Seq(SourceFile(entry, relativePath, absolutePath.toUri))
    }
  }

  private def getAllSourceFiles(entry: SourceEntry, fileSystem: FileSystem, root: Path): Iterator[SourceFile] = {
    if (Files.exists(root)) {
      val sourceMatcher = fileSystem.getPathMatcher("glob:**.{scala,java}")
      Files.walk(root: Path)
        .filter(sourceMatcher.matches)
        .iterator.asScala
        .map { path =>
          val relativePath = root.relativize(path).toString
          SourceFile(entry, relativePath, path.toUri)
        }
    } else Iterator.empty
  }

  private def readAllClassFiles(entry: ClassPathEntry): Seq[ClassFile] = {
    if (entry.isJar)
      withinJarFile(entry.absolutePath) { fileSystem =>
        readAllClassFiles(fileSystem, fileSystem.getPath("/")).toVector
      }
    else
      readAllClassFiles(FileSystems.getDefault, entry.absolutePath).toSeq
  }

  private def readAllClassFiles(fileSystem: FileSystem, root: Path): Iterator[ClassFile] = {
    if (Files.exists(root)) {
      val classMatcher = fileSystem.getPathMatcher("glob:**.class")
      Files.walk(root)
        .filter(classMatcher.matches)
        .iterator.asScala
        .map(readClassFile)
    } else Iterator.empty
  }

  private def readClassFile(path: Path): ClassFile = {
    val inputStream = Files.newInputStream(path)
    val reader = new ClassReader(inputStream)
    val fullyQualifiedName = reader.getClassName.replace('/', '.')
    
    var sourceName = Option.empty[String]
    
    val visitor = new ClassVisitor(Opcodes.ASM7) {
      override def visitSource(source: String, debug: String): Unit = sourceName = Option(source)
    }
    reader.accept(visitor, 0)
    ClassFile(fullyQualifiedName, sourceName, path.toString)
  }

  private def findPackage(sourceFile: SourceFile, fullPackage: String): Boolean = {
    // for "a.b.c" it returns Seq("a.b.c", "b.c", "c")
    // so that we can match on "package a.b.c" or "package b.c" or "package c"
    val nestedPackages = fullPackage.split('.').foldLeft(Seq.empty[String]) { (nestedParts, newPart) =>
      nestedParts.map(outer => s"$outer.$newPart") :+ newPart
    }
    val sourceContent = readSourceContent(sourceFile)
    nestedPackages.exists { `package` =>
      val quotedPackage = Regex.quote(`package`)
      val matcher = s"package\\s+(object\\s+)?$quotedPackage(\\{|:|\\s+)".r
      matcher.findFirstIn(sourceContent).isDefined
    }
  }

  private def readSourceContent(sourceFile: SourceFile): String = {
    withinSourceEntry(sourceFile.entry) { fs => 
      val sourcePath = fs.getPath(sourceFile.relativePath)
      new String(Files.readAllBytes(sourcePath))
    }
  }

  private def withinSourceEntry[T](sourceEntry: SourceEntry)(f: FileSystem => T): T = {
    sourceEntry match {
      case SourceJar(jar) => withinJarFile(jar)(f)
      case _ => f(FileSystems.getDefault)
    }
  }

  private def withinJarFile[T](absolutePath: Path)(f: FileSystem => T): T = {
    val uri = URI.create(s"jar:${absolutePath.toUri}")
    val fileSystem = FileSystems.newFileSystem(uri, new util.HashMap[String, Any])
    try f(fileSystem)
    finally fileSystem.close()
  }
}
