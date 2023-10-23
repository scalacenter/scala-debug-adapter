package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary
import java.lang.reflect.Constructor
import java.lang.reflect.Method

class JavaReflectConstructor(
    constructor: Constructor[?],
    signature: MethodSig,
    extraInfos: ExtraBytecodeInfo,
    loader: JavaReflectLoader
) extends binary.Method:

  override def returnType: Option[binary.Type] = Some(loader.loadClass(classOf[Unit]))

  override def returnTypeName: String = "void"

  override def declaringClass: binary.ClassType =
    loader.loadClass(constructor.getDeclaringClass)

  override def allParameters: Seq[binary.Parameter] =
    constructor.getParameters.map(JavaReflectParameter.apply(_, loader))

  override def name: String = "<init>"

  override def isBridge: Boolean = false

  override def isStatic: Boolean = false

  override def isConstructor: Boolean = true

  override def toString: String = constructor.toString

  override def sourceLines: Seq[binary.SourceLine] = extraInfos.sourceLines

  override def instructions: Seq[binary.Instruction] = extraInfos.instructions
