package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter._
import org.objectweb.asm._

import java.net.URI
import java.nio.file._
import scala.collection.JavaConverters._
import scala.collection.mutable
import ClassEntryLookUp.readSourceContent

import scala.util.matching.Regex

private case class SourceLine(uri: URI, lineNumber: Int)

private case class ClassFile( 
    fullyQualifiedName: String,
    sourceName: Option[String],
    relativePath: String,
    classSystem: ClassSystem
) {
  def className: String = fullyQualifiedName.split('.').last
  def fullPackage: String = fullyQualifiedName.stripSuffix(s".$className")
  def fullPackageAsPath: String = fullPackage.replace(".", "/")
  def folderPath: String = relativePath.stripSuffix(s"/$className.class")
  
  def getBytes(): Array[Byte] = classSystem.bytes(fullyQualifiedName)
}

private class ClassEntryLookUp(
    classNameToClassFile: Map[String, ClassFile],
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

  def getFullyQualifiedClassName(
      sourceUri: URI,
      lineNumber: Int
  ): Option[String] = {
    val line = SourceLine(sourceUri, lineNumber)

    if (!cachedSourceLines.contains(line)) {
      // read and cache line numbers from class files
      sourceUriToClassFiles(sourceUri)
        .groupBy(_.classSystem)
        .foreach { case (classSystem, classFiles) =>
          classSystem.within((_, root) =>
            loadLineNumbers(root, classFiles, sourceUri)
          )
        }
    }

    cachedSourceLines
      .get(line)
      .map { classFiles =>
        // The same breakpoint can stop in different classes
        // We choose the one with the smallest name
        classFiles.map(_.fullyQualifiedName).minBy(_.length)
      }
  }

  private def loadLineNumbers(
      root: Path,
      classFiles: Seq[ClassFile],
      sourceUri: URI
  ): Unit = {
    for (classFile <- classFiles) {
      val path = root.resolve(classFile.relativePath)
      val inputStream = Files.newInputStream(path)
      try {
        val reader = new ClassReader(inputStream)

        val lineNumbers = mutable.Buffer[Int]()

        val visitor = new ClassVisitor(Opcodes.ASM9) {
          override def visitMethod(
              access: Int,
              name: String,
              desc: String,
              signature: String,
              exceptions: Array[String]
          ): MethodVisitor = {
            new MethodVisitor(Opcodes.ASM9) {
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
      } finally {
        inputStream.close()
      }
    }
  }

  def getSourceContent(sourceUri: URI): Option[String] = {
    sourceUriToSourceFile.get(sourceUri).flatMap(readSourceContent)
  }

  def getSourceFile(fqcn: String): Option[URI] = {
    classNameToSourceFile.get(fqcn).map(_.uri)
  }

  def getClassFile(className: String): Option[ClassFile] = {
    classNameToClassFile.get(className)
  }
}

private object ClassEntryLookUp {
  private def empty: ClassEntryLookUp =
    new ClassEntryLookUp(Map.empty, Map.empty, Map.empty, Map.empty, Seq.empty, Seq.empty)

  private[internal] def apply(entry: ClassEntry): ClassEntryLookUp = {
    val sourceFiles =
      entry.sourceEntries.flatMap(SourceEntryLookUp.getAllSourceFiles)
    ClassEntryLookUp(entry.classSystems, sourceFiles)
  }

  def apply(
      classSystems: Seq[ClassSystem],
      sourceFiles: Seq[SourceFile]
  ): ClassEntryLookUp = {
    if (sourceFiles.isEmpty) ClassEntryLookUp.empty
    else {
      val classFiles = classSystems.flatMap { classSystem =>
        classSystem
          .within(readAllClassFiles(classSystem))
          .getOrElse(Vector.empty)
      }

      // println("Qualif name : " + classFiles.head.fullyQualifiedName + "\nName: " + classFiles.head.className + "\nPath: " + classFiles.head.relativePath)
      // Console.flush()
      for(c <- classFiles if c.fullyQualifiedName.contains("com.sun.tools")) println(c.fullyQualifiedName)
      
      // println(classFiles.forall(c => !c.fullyQualifiedName.contains("com.sun.tools")))
      val classNameToClassFile = classFiles.map(c => (c.fullyQualifiedName, c)).toMap

      val sourceUriToSourceFile = sourceFiles.map(f => (f.uri, f)).toMap
      val sourceNameToSourceFile = sourceFiles.groupBy(f => f.fileName)

      val classNameToSourceFile = mutable.Map[String, SourceFile]()
      val sourceUriToClassFiles = mutable.Map[URI, Seq[ClassFile]]()
      val orphanClassFiles = mutable.Buffer[ClassFile]()
      val missingSourceFileClassFiles = mutable.Buffer[ClassFile]()

      for (classFile <- classFiles) {
        def recordSourceFile(sourceFile: SourceFile): Unit = {
          classNameToSourceFile.put(classFile.fullyQualifiedName, sourceFile)
          sourceUriToClassFiles.update(
            sourceFile.uri,
            sourceUriToClassFiles.getOrElse(
              sourceFile.uri,
              Seq.empty
            ) :+ classFile
          )
        }

        classFile.sourceName
          .flatMap(sourceNameToSourceFile.get)
          .getOrElse(Seq.empty)
          .toList match {
          case Nil =>
            // the source name is missing from the class file
            // or the source file is missing from the source entry
            missingSourceFileClassFiles.append(classFile)
          case sourceFile :: Nil =>
            // there is only one file with that name, it must be the right one
            // even if its relative path does not match the class package
            recordSourceFile(sourceFile)
          case manySourceFiles =>
            // there are several files with the same name
            // we find the one whose relative path matches the class package
            manySourceFiles.find(f =>
              f.folderPath == classFile.folderPath
            ) match {
              case Some(sourceFile) => recordSourceFile(sourceFile)
              case None =>
                // in some modules of the java 9+ runtimes, the pattern of the path
                // of the source files is <module>/<project>/src/<package>/<fileName>.java
                // we find the package name by splitting at "src/"
                manySourceFiles
                  .filter(_.folderPath.contains("src/"))
                  .find(f =>
                    f.folderPath
                      .split("src/")
                      .last == classFile.fullPackageAsPath
                  ) match {
                  case Some(sourceFile) => recordSourceFile(sourceFile)
                  case None =>
                    // there is no source file with the correct relative path
                    // so we try to find the right package declaration in each file
                    // it would be very unfortunate that 2 sources file with the same name
                    // declare the same package.
                    manySourceFiles.filter(s =>
                      findPackage(s, classFile.fullPackage)
                    ) match {
                      case sourceFile :: Nil =>
                        recordSourceFile(sourceFile)
                      case _ =>
                        orphanClassFiles.append(classFile)
                    }
                }
            }
        }
      }

      new ClassEntryLookUp(
        classNameToClassFile,
        sourceUriToSourceFile,
        sourceUriToClassFiles.toMap,
        classNameToSourceFile.toMap,
        missingSourceFileClassFiles,
        orphanClassFiles
      )
    }
  }

  private def readAllClassFiles(
      classSystem: ClassSystem
  )(fileSystem: FileSystem, root: Path): Vector[ClassFile] = {
    if (Files.exists(root)) {
      val classMatcher = fileSystem.getPathMatcher("glob:**.class")
      Files
        .walk(root)
        .filter(classMatcher.matches)
        .iterator
        .asScala
        .map(readClassFile(classSystem, root))
        .toVector
    } else Vector.empty
  }

  private def readClassFile(classSystem: ClassSystem, root: Path)(
      path: Path
  ): ClassFile = {
    val inputStream = Files.newInputStream(path)
    try {
      val reader = new ClassReader(inputStream)
      val fullyQualifiedName = reader.getClassName.replace('/', '.')

      var sourceName = Option.empty[String]

      val visitor = new ClassVisitor(Opcodes.ASM9) {
        override def visitSource(source: String, debug: String): Unit =
          sourceName = Option(source)
      }
      reader.accept(visitor, 0)
      val relativePath = root.relativize(path)
      ClassFile(
        fullyQualifiedName,
        sourceName,
        relativePath.toString,
        classSystem
      )
    } finally {
      inputStream.close()
    }
  }

  private def findPackage(
      sourceFile: SourceFile,
      fullPackage: String
  ): Boolean = {
    // for "a.b.c" it returns Seq("a.b.c", "b.c", "c")
    // so that we can match on "package a.b.c" or "package b.c" or "package c"
    val nestedPackages = fullPackage.split('.').foldLeft(Seq.empty[String]) {
      (nestedParts, newPart) =>
        nestedParts.map(outer => s"$outer.$newPart") :+ newPart
    }
    val sourceContent = readSourceContent(sourceFile)
    nestedPackages.exists { `package` =>
      val quotedPackage = Regex.quote(`package`)
      val matcher = s"package\\s+(object\\s+)?$quotedPackage(\\{|:|;|\\s+)".r
      sourceContent.exists(matcher.findFirstIn(_).isDefined)
    }
  }

  private def readSourceContent(sourceFile: SourceFile): Option[String] = {
    withinSourceEntry(sourceFile.entry) { root =>
      val sourcePath = root.resolve(sourceFile.relativePath)
      new String(Files.readAllBytes(sourcePath))
    }
  }

  private def withinSourceEntry[T](
      sourceEntry: SourceEntry
  )(f: Path => T): Option[T] = {
    sourceEntry match {
      case SourceJar(jar) => IO.withinJarFile(jar)(fs => f(fs.getPath("/")))
      case SourceDirectory(dir) => Some(f(dir))
      case StandaloneSourceFile(absolutePath, _) =>
        Some(f(absolutePath.getParent))
    }
  }
}
