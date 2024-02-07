package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.*

import scala.jdk.CollectionConverters.*

private[evaluator] class JdiObject(
    val reference: ObjectReference,
    thread: ThreadReference
) extends JdiValue(reference, thread) {
  def getField(field: Field): JdiValue =
    JdiValue(reference.getValue(field), thread)

  def getField(name: String): JdiValue = {
    val field = reference.referenceType.fieldByName(name)
    JdiValue(reference.getValue(field), thread)
  }

  def invoke(methodName: String, args: Seq[JdiValue]): Safe[JdiValue] = {
    val m = reference.referenceType.methodsByName(methodName).get(0)
    invoke(m, args)
  }

  def invoke(methodName: String, signature: String, args: Seq[JdiValue]): Safe[JdiValue] = {
    val m = reference.referenceType.methodsByName(methodName, signature).get(0)
    invoke(m, args)
  }

  def classObject: JdiClass = JdiClass(reference.referenceType, thread)
  def classLoader: JdiClassLoader = JdiClassLoader(reference.referenceType.classLoader, thread)

  def invoke(method: Method, args: Seq[JdiValue]): Safe[JdiValue] =
    Safe(reference.invokeMethod(thread, method, args.map(_.value).asJava, ObjectReference.INVOKE_SINGLE_THREADED))
      .map(JdiValue(_, thread))
      .recoverWith(wrapInvocationException)

  protected val wrapInvocationException: PartialFunction[Throwable, Safe[Nothing]] = {
    case invocationException: InvocationException =>
      for {
        exception <- Safe(invocationException.exception).map(JdiObject(_, thread))
        message <- exception.invoke("toString", List()).map(_.asString.stringValue).recover { case _ => "" }
      } yield throw new RuntimeException(message, Some(exception))
  }

  // we use a Seq instead of a Map because the ScalaEvaluator rely on the order of the fields
  def fields: Seq[(String, JdiValue)] =
    reference.referenceType.fields.asScala.toSeq
      .map(f => (f.name, JdiValue(reference.getValue(f), thread)))
}

private[internal] object JdiObject {
  def apply(value: Value, thread: ThreadReference): JdiObject =
    new JdiObject(value.asInstanceOf[ObjectReference], thread)
}
