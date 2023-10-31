package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary

import java.lang.reflect.Method
import java.lang.reflect.Modifier
import ch.epfl.scala.debugadapter.internal.binary.MethodSig

class JavaReflectMethod(
    method: Method,
    val signature: MethodSig,
    extraInfos: ExtraBytecodeInfo,
    loader: JavaReflectLoader
) extends binary.Method:

  override def returnType: Option[binary.Type] =
    Option(method.getReturnType).map(loader.loadClass)

  override def returnTypeName: String = method.getReturnType.getName

  override def declaringClass: binary.ClassType =
    loader.loadClass(method.getDeclaringClass)

  override def allParameters: Seq[binary.Parameter] =
    method.getParameters.map(JavaReflectParameter.apply(_, loader))

  override def name: String = method.getName

  override def isStatic: Boolean = Modifier.isStatic(method.getModifiers)

  override def toString: String = 
    val span =
      if sourceLines.size > 2 then Seq(sourceLines.min, sourceLines.max)
      else sourceLines
    s"$method (${span.mkString(", ")})"

  override def isBridge: Boolean = method.isBridge

  override def isConstructor: Boolean = false

  override def sourceLines: Seq[binary.SourceLine] = extraInfos.sourceLines

  override def instructions: Seq[binary.Instruction] = extraInfos.instructions
