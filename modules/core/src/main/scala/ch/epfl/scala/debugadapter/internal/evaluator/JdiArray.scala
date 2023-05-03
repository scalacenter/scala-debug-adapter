package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.ArrayReference
import com.sun.jdi.ThreadReference
import com.sun.jdi.Value

import scala.jdk.CollectionConverters.*

class JdiArray(arrayRef: ArrayReference, thread: ThreadReference) extends JdiObject(arrayRef, thread) {
  def setValue(index: Int, value: Value): Unit =
    arrayRef.setValue(index, value)

  def setValues(values: Seq[JdiValue]): Unit = arrayRef.setValues(values.map(_.value).asJava)

  def getValues: Seq[JdiValue] = arrayRef.getValues.asScala.toSeq.map(JdiValue(_, thread))
}

object JdiArray {
  def apply(arrayValue: Value, thread: ThreadReference): JdiArray =
    new JdiArray(arrayValue.asInstanceOf[ArrayReference], thread)
}
