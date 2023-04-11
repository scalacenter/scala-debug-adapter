package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.ClassLoaderReference
import com.sun.jdi.LocalVariable
import com.sun.jdi.StackFrame
import com.sun.jdi.ThreadReference
import com.sun.jdi.ReferenceType
import com.sun.jdi.PrimitiveType
import com.sun.jdi.{BooleanType, ByteType, CharType, DoubleType, FloatType, IntegerType, LongType, ShortType}

import scala.jdk.CollectionConverters.*

final case class JdiFrame(thread: ThreadReference, depth: Int) {
  def current(): StackFrame = thread.frame(depth)

  // it's a Safe because it can fail, but it could also be a Try
  def classLoader(): Safe[JdiClassLoader] = {
    def getClassLoaderRecursively(depth: Int): Option[ClassLoaderReference] =
      if (depth == thread.frameCount) None
      else {
        Option(thread.frame(depth).location.method.declaringType.classLoader)
          .orElse(getClassLoaderRecursively(depth + 1))
      }

    Safe {
      val classLoader = getClassLoaderRecursively(depth)
        .getOrElse(throw new Exception("Cannot find any class loader in the stack trace"))
      JdiClassLoader(classLoader, thread)
    }
  }

  // this object can be null
  lazy val thisObject: Option[JdiObject] =
    Option(current().thisObject).map(JdiObject(_, thread))

  def variables(): Seq[LocalVariable] =
    current().visibleVariables.asScala.toSeq

  def variablesAndValues(): Seq[(LocalVariable, JdiValue)] =
    variables().map(v => v -> JdiValue(current().getValue(v), thread))

  @inline def variableByName(name: String): Option[LocalVariable] =
    variables().find(_.name == name)

  @inline def variableValue(variable: LocalVariable): JdiValue =
    JdiValue(current().getValue(variable), thread)

  def setVariable(variable: LocalVariable, value: JdiValue): Unit =
    current().setValue(variable, value.value)

  def getPrimitiveBoxedClass(pt: PrimitiveType): ReferenceType = {
    val vm = current().virtualMachine()
    def cls(name: String) = vm.classesByName(name).get(0)
    pt match {
      case _: BooleanType => cls("java.lang.Boolean")
      case _: ByteType => cls("java.lang.Byte")
      case _: CharType => cls("java.lang.Character")
      case _: DoubleType => cls("java.lang.Double")
      case _: FloatType => cls("java.lang.Float")
      case _: IntegerType => cls("java.lang.Integer")
      case _: LongType => cls("java.lang.Long")
      case _: ShortType => cls("java.lang.Short")
    }
  }
}
