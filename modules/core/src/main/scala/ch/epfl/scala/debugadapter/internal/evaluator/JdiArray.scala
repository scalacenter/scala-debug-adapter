package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.ArrayReference
import com.sun.jdi.ThreadReference
import com.sun.jdi.Value

import scala.jdk.CollectionConverters.*

class JdiArray(reference: ArrayReference, thread: ThreadReference) extends JdiObject(reference, thread) {
  def setValue(index: Int, value: Value): Unit =
    reference.setValue(index, value)

  def setValues(values: Seq[JdiValue]): Unit = reference.setValues(values.map(_.value).asJava)

  def getValues: Seq[JdiValue] = reference.getValues.asScala.toSeq.map(JdiValue(_, thread))
}

object JdiArray {
  def apply(value: Value, thread: ThreadReference): JdiArray =
    new JdiArray(value.asInstanceOf[ArrayReference], thread)
}
