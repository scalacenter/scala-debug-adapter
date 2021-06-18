package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.DebuggeeRunner
import ch.epfl.scala.debugadapter.Logger

import com.microsoft.java.debug.core.adapter.ISourceLookUpProvider

import org.objectweb.asm.{ClassReader, ClassVisitor, Label, MethodVisitor, Opcodes}

import java.net.URI
import java.nio.file.{Path, Files}

import scala.util.control.NonFatal
import scala.collection.mutable


private[debugadapter] final class SourceLookUpProvider(runner: DebuggeeRunner, logger: Logger) extends ISourceLookUpProvider {
  override def supportsRealtimeBreakpointVerification(): Boolean = true
  override def getSourceFileURI(fqn: String, path: String): String = path
  override def getSourceContents(uri: String): String = ""

  override def getFullyQualifiedName(uriRepr: String, lines: Array[Int], columns: Array[Int]): Array[String] = {
    val uri = URI.create(uriRepr)
    uri.getScheme match {
      case "dap-fqcn" =>
        val resolvedName = uri.getSchemeSpecificPart
        lines.map(_ => resolvedName)
      case _ =>
        val originSource = java.nio.file.Paths.get(uri)
        val classFiles = runner.classFilesMappedTo(originSource, lines, columns)
        lines.map(line => targetClass(line, classFiles))
    }
  }

  private def targetClass(line: Int, classFiles: List[Path]): String = {
    classFiles
      .flatMap { classFile =>
        try {
          val bytes = Files.readAllBytes(classFile)
          val reader = new ClassReader(bytes)
          if (debugLineNumbers(reader).contains(line)) {
            val className = reader.getClassName().replace('/', '.')
            Some(className)
          } else None
        } catch {
          case NonFatal(cause) =>
            logger.error(s"Failed to parse debug line numbers in class file $classFile!")
            logger.trace(cause)
            None
        }
      }
      .headOption
      .getOrElse(null)
  }

  private def debugLineNumbers(reader: ClassReader): Set[Int] = {
    var acc = mutable.Set[Int]()
    val visitor = new ClassVisitor(Opcodes.ASM7) {
      override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
        new MethodVisitor(Opcodes.ASM7) {
          override def visitLineNumber(line: Int, start: Label): Unit = {
            acc.add(line)
          }
        }
      }
    }
    reader.accept(visitor, 0)
    acc.toSet
  }
}