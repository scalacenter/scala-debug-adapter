package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._

private[internal] class JdiValue(val value: Value, val thread: ThreadReference) {
  def asObject: JdiObject = JdiObject(value.asInstanceOf[ObjectReference], thread)
  def asClass: JdiClass = JdiClass(value.asInstanceOf[ClassObjectReference], thread)
  def asClassLoader: JdiClassLoader = JdiClassLoader(value.asInstanceOf[ClassLoaderReference], thread)
  def asArray: JdiArray = JdiArray(value.asInstanceOf[ArrayReference], thread)
  def asString: JdiString = new JdiString(value.asInstanceOf[StringReference], thread)

  def unboxIfPrimitive: Safe[JdiValue] = {
    value match {
      case ref: ObjectReference =>
        val typeName = ref.referenceType.name
        JdiValue.unboxMethods
          .get(typeName)
          .map(methodName => asObject.invoke(methodName, Nil))
          .getOrElse(Safe(this))
      case _ => Safe(this)
    }
  }

  def derefIfRef: JdiValue =
    value match {
      case ref: ObjectReference if JdiValue.refTypes.contains(ref.referenceType.name) =>
        asObject.getField("elem")
      case _ => this
    }
}

object JdiValue {
  def apply(value: Value, thread: ThreadReference): JdiValue = new JdiValue(value, thread)

  private val unboxMethods = Map(
    "java.lang.Boolean" -> "booleanValue",
    "java.lang.Byte" -> "byteValue",
    "java.lang.Character" -> "charValue",
    "java.lang.Double" -> "doubleValue",
    "java.lang.Float" -> "floatValue",
    "java.lang.Integer" -> "intValue",
    "java.lang.Long" -> "longValue",
    "java.lang.Short" -> "shortValue"
  )

  private val refTypes = Set(
    "scala.runtime.BooleanRef",
    "scala.runtime.ByteRef",
    "scala.runtime.CharRef",
    "scala.runtime.DoubleRef",
    "scala.runtime.FloatRef",
    "scala.runtime.IntRef",
    "scala.runtime.LongRef",
    "scala.runtime.ShortRef",
    "scala.runtime.ObjectRef"
  )
}
