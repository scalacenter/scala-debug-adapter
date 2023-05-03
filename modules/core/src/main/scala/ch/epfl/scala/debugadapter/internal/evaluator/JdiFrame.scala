package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.ClassLoaderReference
import com.sun.jdi.LocalVariable
import com.sun.jdi.StackFrame
import com.sun.jdi.ThreadReference

import scala.collection.JavaConverters._

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
        .getOrElse(throw new Exception("Cannot find any classloader in the stack trace"))
      JdiClassLoader(classLoader, thread)
    }
  }

  // this object can be null
  def thisObject(): Option[JdiObject] =
    Option(current().thisObject).map(JdiObject(_, thread))

  def variables(): Seq[LocalVariable] =
    current().visibleVariables.asScala.toSeq

  def variablesAndValues(): Seq[(LocalVariable, JdiValue)] = {
    val frame = current()
    variables().map(v => v -> JdiValue(frame.getValue(v), thread))
  }

  def setVariable(variable: LocalVariable, value: JdiValue): Unit =
    current().setValue(variable, value.value)
}
