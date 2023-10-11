package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary.*

import scala.collection.mutable
import org.objectweb.asm
import java.io.IOException

class JavaReflectLoader(classLoader: ClassLoader, loadExtraInfo: Boolean = true):
  private val loadedClasses: mutable.Map[Class[?], JavaReflectClass] = mutable.Map.empty

  def loadClass(cls: Class[?]): JavaReflectClass =
    loadedClasses.getOrElseUpdate(cls, doLoadClass(cls))

  def loadClass(name: String): JavaReflectClass =
    val cls = classLoader.loadClass(name)
    loadClass(cls)

  private def doLoadClass(cls: Class[?]): JavaReflectClass =
    val lines =
      if loadExtraInfo && !cls.isPrimitive && !cls.isArray then
        try
          val name = cls.getName
          val inputStream = classLoader.getResourceAsStream(name.replace('.', '/') + ".class")
          val asmReader = new asm.ClassReader(inputStream)
          getExtraInfo(asmReader)
        catch case _: IOException => Map.empty
      else Map.empty
    JavaReflectClass(cls, lines, this)

  private def getExtraInfo(reader: asm.ClassReader): Map[MethodSig, ExtraBytecodeInfo] =
    assert(loadExtraInfo)
    val extraInfos = mutable.Map.empty[MethodSig, ExtraBytecodeInfo]
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
            val lines = mutable.Buffer.empty[Int]
            val instructions = mutable.Buffer.empty[Instruction]
            override def visitLineNumber(line: Int, start: asm.Label): Unit =
              lines += line
            override def visitMethodInsn(
                opcode: Int,
                owner: String,
                name: String,
                descriptor: String,
                isInterface: Boolean
            ): Unit =
              instructions += Instruction.Method(opcode, owner.replace('/', '.'), name, descriptor, isInterface)
            override def visitEnd(): Unit =
              val sourceLines = lines.toSeq.distinct.sorted.map(SourceLine(_))
              extraInfos += MethodSig(name, descriptor) -> ExtraBytecodeInfo(sourceLines, instructions.toSeq)
    reader.accept(visitor, asm.Opcodes.ASM9)
    extraInfos.toMap
