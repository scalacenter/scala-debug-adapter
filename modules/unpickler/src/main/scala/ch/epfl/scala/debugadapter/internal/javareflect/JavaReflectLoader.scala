package ch.epfl.scala.debugadapter.internal.javareflect

import scala.collection.mutable
import org.objectweb.asm
import java.io.IOException

class JavaReflectLoader(classLoader: ClassLoader, readSourceLines: Boolean = true):
  private val loadedClasses: mutable.Map[Class[?], JavaReflectClass] = mutable.Map.empty

  def loadClass(cls: Class[?]): JavaReflectClass =
    loadedClasses.getOrElseUpdate(cls, doLoadClass(cls))

  def loadClass(name: String): JavaReflectClass =
    val cls = classLoader.loadClass(name)
    loadClass(cls)

  private def doLoadClass(cls: Class[?]): JavaReflectClass =
    val lines =
      if readSourceLines && !cls.isPrimitive && !cls.isArray then
        try
          val name = cls.getName
          val inputStream = classLoader.getResourceAsStream(name.replace('.', '/') + ".class")
          val asmReader = new asm.ClassReader(inputStream)
          getLineNumbers(asmReader)
        catch case _: IOException => Map.empty
      else Map.empty
    JavaReflectClass(cls, lines, this)

  private def getLineNumbers(reader: asm.ClassReader): Map[MethodSig, Seq[Int]] =
    assert(readSourceLines)
    var linesMap = Map.empty[MethodSig, Seq[Int]]
    val visitor =
      new asm.ClassVisitor(asm.Opcodes.ASM9):
        override def visitMethod(
            access: Int,
            name: String,
            descriptor: String,
            signature: String,
            exceptions: Array[String]
        ): asm.MethodVisitor =
          new asm.MethodVisitor(asm.Opcodes.ASM9):
            val lines = mutable.Set.empty[Int]
            override def visitLineNumber(line: Int, start: asm.Label): Unit =
              lines += line
            override def visitEnd(): Unit =
              val span = if lines.size > 1 then Seq(lines.min, lines.max) else lines.toSeq
              linesMap = linesMap + (MethodSig(name, descriptor) -> span)
    reader.accept(visitor, asm.Opcodes.ASM9)
    linesMap
