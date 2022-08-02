package ch.epfl.scala.debugadapter.internal

import java.lang.StringBuilder
import java.nio.charset.StandardCharsets.UTF_8

import scala.reflect.internal.pickling.ByteCodecs
import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.AnnotationVisitor
import org.objectweb.asm.Opcodes

import ch.epfl.scala.debugadapter.internal.scalasig._

/**
 * Originally copied from https://github.com/JetBrains/intellij-scala
 * https://github.com/JetBrains/intellij-scala/blob/074e8f98d9789b3e7def3ade8d39e7ae770beccf/scala/decompiler/src/org/jetbrains/plugins/scala/decompiler/scalasig/Decompiler.scala
 */
object Decompiler {
  private val BYTES_VALUE = "bytes"
  private val SCALA_SIG_ANNOTATION: String = "Lscala/reflect/ScalaSignature;"
  private val SCALA_LONG_SIG_ANNOTATION: String =
    "Lscala/reflect/ScalaLongSignature;"

  private val ScalaSigBytes = "ScalaSig".getBytes(UTF_8)

  def decompile(classFile: ClassFile): Option[ScalaSig] = {
    val bytes = classFile.readBytes()
    decompile(bytes, classFile.fullyQualifiedName)
  }

  private[internal] def decompile(
      classBytes: Array[Byte],
      fqcn: String
  ): Option[ScalaSig] = {
    if (!containsMarker(classBytes)) return None

    val reader = new ClassReader(classBytes)

    val scalaAnnotations = scala.collection.mutable.Buffer[String]()

    val emptyVisitor = new AnnotationVisitor(Opcodes.ASM9) {}
    val scalaVisitor = new AnnotationVisitor(Opcodes.ASM9) {
      override def visitArray(name: String): AnnotationVisitor = {
        if (name == "bytes") {
          new AnnotationVisitor(Opcodes.ASM9) {
            override def visit(name: String, value: Any): Unit = {
              scalaAnnotations += value.asInstanceOf[String]
            }
          }
        } else {
          emptyVisitor
        }
      }

      override def visit(name: String, value: Any): Unit = {
        if (name == "bytes") {
          scalaAnnotations += value.asInstanceOf[String]
        }
      }

    }

    val visitor = new ClassVisitor(Opcodes.ASM9) {
      override def visitAnnotation(
          descriptor: String,
          visible: Boolean
      ): AnnotationVisitor = {
        descriptor match {
          case SCALA_SIG_ANNOTATION | SCALA_LONG_SIG_ANNOTATION =>
            scalaVisitor
          case _ => emptyVisitor
        }
      }
    }

    reader.accept(visitor, 256)

    if (scalaAnnotations.isEmpty) None
    else {
      val decoded = decode(scalaAnnotations.toList.map(_.getBytes()))
      Some(Parser.parseScalaSig(decoded, fqcn))
    }
  }

  private def tryDecompileSigFile(
      fileName: String,
      bytes: Array[Byte]
  ): Option[String] = {
    try {
      val scalaSig = Parser.parseScalaSig(bytes, fileName)
      val isPackageObject = fileName == "package.sig"
      val sourceNameGuess = fileName.stripSuffix(".sig") + ".scala"

      decompiledText(scalaSig, fileName, isPackageObject)
    } catch {
      case e: Exception =>
        // not every `.sig` file is a scala signature file
        None
    }
  }

  private def decode(strings: List[Array[Byte]]) = {
    val bytes = Array.concat(strings: _*)
    ByteCodecs.decode(bytes)
    bytes
  }

  def decompiledText(
      scalaSig: ScalaSig,
      className: String,
      isPackageObject: Boolean
  ) =
    try {
      val printer = new ScalaSigPrinter(new StringBuilder)

      def findPath(symbol: Symbol) = symbol.name match {
        case "<empty>" => None
        case _ =>
          val path = symbol.path
          if (isPackageObject) {
            path.lastIndexOf(".") match {
              case -1 | 0 => None
              case index => Some(path.substring(0, index))
            }
          } else Some(path)
      }

      val symbols = scalaSig.topLevelClasses ++ scalaSig.topLevelObjects

      // Print package with special treatment for package objects
      for {
        symbol <- symbols.headOption
        parent <- symbol.parent
        path <- findPath(parent)
        packageName = ScalaSigPrinter.processName(path)
      } {
        printer.print("package ")
        printer.print(packageName)
        printer.print("\n")
      }

      // Print classes
      for (symbol <- symbols) {
        printer.printSymbol(symbol)
      }

      Some(printer.result)
    } catch {
      case e: ScalaDecompilerException =>
        // Log.warn(s"Error decompiling class $className, ${e.getMessage}")
        None
      case cause: Exception =>
        // Log.error(s"Error decompiling class $className", cause)
        None
    }

  private def containsMarker(text: Array[Byte]): Boolean = {
    val arrayLength = ScalaSigBytes.length
    text.length - arrayLength match {
      case delta if delta >= 1 =>
        var wordStartIdx = 0
        var innerIdx = 0

        while (wordStartIdx <= delta) {
          while (
            innerIdx < arrayLength && text(
              wordStartIdx + innerIdx
            ) == ScalaSigBytes(innerIdx)
          ) {
            innerIdx += 1
          }
          if (innerIdx == arrayLength) return true
          else {
            wordStartIdx += 1
            innerIdx = 0
          }
        }

        false
      case _ => false
    }
  }

}
