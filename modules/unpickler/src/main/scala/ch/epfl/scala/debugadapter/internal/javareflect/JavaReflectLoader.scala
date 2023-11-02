package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary.*

import scala.collection.mutable
import org.objectweb.asm
import java.io.IOException
import ch.epfl.scala.debugadapter.internal.binary.MethodSig
import java.net.URLClassLoader
import java.nio.file.Path

class JavaReflectLoader(classLoader: ClassLoader, loadExtraInfo: Boolean) extends BinaryClassLoader:
  private val loadedClasses: mutable.Map[Class[?], JavaReflectClass] = mutable.Map.empty

  def loadClass(cls: Class[?]): JavaReflectClass =
    loadedClasses.getOrElseUpdate(cls, doLoadClass(cls))

  override def loadClass(name: String): JavaReflectClass =
    val cls = classLoader.loadClass(name)
    loadClass(cls)

  private def doLoadClass(cls: Class[?]): JavaReflectClass =
    val extraInfo =
      if loadExtraInfo && !cls.isPrimitive && !cls.isArray then
        try
          val name = cls.getName
          val inputStream = classLoader.getResourceAsStream(name.replace('.', '/') + ".class")
          val asmReader = new asm.ClassReader(inputStream)
          getExtraInfo(asmReader)
        catch case _: IOException => ExtraClassInfo.empty
      else ExtraClassInfo.empty
    JavaReflectClass(cls, extraInfo, this)

  private def getExtraInfo(reader: asm.ClassReader): ExtraClassInfo =
    assert(loadExtraInfo)
    var sourceName: String = ""
    var allLines = mutable.Set.empty[Int]
    val extraInfos = mutable.Map.empty[MethodSig, ExtraMethodInfo]
    val visitor =
      new asm.ClassVisitor(asm.Opcodes.ASM9):
        override def visitSource(source: String, debug: String): Unit = sourceName = source
        override def visitMethod(
            access: Int,
            name: String,
            descriptor: String,
            signature: String,
            exceptions: Array[String]
        ): asm.MethodVisitor =
          new asm.MethodVisitor(asm.Opcodes.ASM9):
            val lines = mutable.Set.empty[Int]
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
              allLines ++= lines
              val sourceLines = Option.when(sourceName.nonEmpty)(SourceLines(sourceName, lines.toSeq))
              extraInfos += MethodSig(name, descriptor) -> ExtraMethodInfo(sourceLines, instructions.toSeq)
    reader.accept(visitor, asm.Opcodes.ASM9)
    val sourceLines = Option.when(sourceName.nonEmpty)(SourceLines(sourceName, allLines.toSeq))
    ExtraClassInfo(sourceLines, extraInfos.toMap)

object JavaReflectLoader:
  def apply(classPath: Seq[Path], loadExtraInfo: Boolean = true): JavaReflectLoader =
    val classLoader = URLClassLoader(classPath.map(_.toUri.toURL).toArray, null: ClassLoader)
    new JavaReflectLoader(classLoader, loadExtraInfo)
