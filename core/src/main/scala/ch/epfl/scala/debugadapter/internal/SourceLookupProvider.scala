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

  override def getFullyQualifiedName(
      uriRepr: String,
      lines: Array[Int],
      columns: Array[Int]
  ): Array[String] = {
    val uri = URI.create(uriRepr)
    if (uri.getScheme == "dap-fqcn") {
      val resolvedName = uri.getSchemeSpecificPart
      lines.map(_ => resolvedName)
    } else {
      val originSource = java.nio.file.Paths.get(uri)
      val classFiles = runner.classFilesMappedTo(originSource, lines, columns)
      lines.map(line => definingName(line, classFiles))
    }
  }

  private def collectLineNumbers(
      reader: ClassReader,
      lines: mutable.HashMap[Int, String]
  ): Unit = {
    val className = reader.getClassName
    val visitor = new ClassVisitor(Opcodes.ASM7) {
      override def visitMethod(
          access: Int,
          name: String,
          desc: String,
          signature: String,
          exceptions: Array[String]
      ): MethodVisitor = {
        new MethodVisitor(Opcodes.ASM7) {
          override def visitLineNumber(line: Int, start: Label): Unit = {
            lines.+=(line -> className)
          }
        }
      }
    }

    reader.accept(visitor, 0)
  }

  /**
   * Parses all class files defined in a compilation unit and returns the
   * candidate that defines an instruction at line `line`.
   */
  private def definingName(line: Int, candidates: Seq[Path]): String = {
    var firstName: String = ""
    val lines = new mutable.HashMap[Int, String]()
    candidates.foreach { classFile =>
      try {
        val bytes = Files.readAllBytes(classFile)
        val reader = new ClassReader(bytes)
        collectLineNumbers(reader, lines)
        if (firstName.isEmpty) {
          firstName = reader.getClassName
        }
      } catch {
        case NonFatal(t) =>
          logger.error(s"Failed to parse debug line numbers in class file $classFile!")
          logger.trace(t)
      }
    }

    lines
      .get(line)
      .orElse {
        val closestLineOrdering = new scala.math.Ordering[(Int, String)] {
          def compare(x: (Int, String), y: (Int, String)): Int = {
            val line1 = x._1
            val line2 = y._1
            val intOrd = implicitly[scala.math.Ordering[Int]]
            val result = intOrd.compare(scala.math.abs(line1), scala.math.abs(line2))
            if (result != 0) result
            else {
              if (line1 < line2) line2
              else line1
            }
          }
        }

        /*
          * In case the line where the breakpoint is set isn't found in the code
          * array -- a very rare event, but possible -- we pick the name whose
          * index is closest to the target `line`, both from below and above.
          * For example, if line is 8 it will prefer to pick 7 over 9, but 9
          * over 10, as the algorithm tries to go for the instruction closest
          * to the target line. This strategy is defined in the above ordering.
          */
        lines.iterator
          .map { case (codeLine, name) => (line - codeLine) -> name }
          .toList
          .sorted(closestLineOrdering)
          .headOption
          .map(_._2)
      }
      // Returns first name or empty if previous strategies failed
      .getOrElse(firstName)
  }
}