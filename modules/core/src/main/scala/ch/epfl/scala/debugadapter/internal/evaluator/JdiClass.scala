package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.*

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

private[internal] class JdiClass(
    val cls: ClassType,
    thread: ThreadReference
) extends JdiObject(cls.classObject, thread) {

  def initialized: Boolean = cls.isInitialized
  def className: String = cls.name
  override def classLoader: JdiClassLoader = JdiClassLoader(cls.classLoader, thread)

  def newInstance(args: Seq[JdiValue]): Safe[JdiObject] = {
    val ctr = cls.methodsByName("<init>").get(0)
    newInstance(ctr, args)
  }

  def newInstance(signature: String, args: Seq[JdiValue]): Safe[JdiObject] = {
    val ctr = cls.methodsByName("<init>", signature).get(0)
    newInstance(ctr, args)
  }

  def newInstance(ctr: Method, args: Seq[JdiValue]): Safe[JdiObject] =
    for {
      _ <- prepareMethod(ctr)
      instance <- Safe(cls.newInstance(thread, ctr, args.map(_.value).asJava, ObjectReference.INVOKE_SINGLE_THREADED))
        .recoverWith(wrapInvocationException(thread))
    } yield JdiObject(instance, thread)

  // Load the argument types of the method to avoid ClassNotLoadedException
  // TODO Should we use this method before all invocations: methods, ctrs, fields
  private def prepareMethod(method: Method): Safe[Unit] = {
    def loadArgumentsRecursively(): Safe[Unit] = {
      try {
        method.argumentTypes()
        Safe(())
      } catch {
        case exception: ClassNotLoadedException =>
          val className = exception.className.stripSuffix("[]").replace('/', '.')
          classLoader.loadClass(className).flatMap(_ => loadArgumentsRecursively())
        case NonFatal(cause) => Safe(throw cause)
      }
    }
    loadArgumentsRecursively()
  }

  def getStaticField(fieldName: String): Safe[JdiValue] =
    Safe(cls.getValue(cls.fieldByName(fieldName))).map(JdiValue(_, thread))

  def invokeStatic(methodName: String, args: Seq[JdiValue]): Safe[JdiValue] = {
    val method = cls.methodsByName(methodName).get(0)
    invokeStatic(method, args)
  }

  def invokeStatic(methodName: String, signature: String, args: Seq[JdiValue]): Safe[JdiValue] = {
    val method = cls.methodsByName(methodName, signature).get(0)
    invokeStatic(method, args)
  }

  def invokeStatic(method: Method, args: Seq[JdiValue]): Safe[JdiValue] =
    Safe(cls.invokeMethod(thread, method, args.map(_.value).asJava, ObjectReference.INVOKE_SINGLE_THREADED))
      .map(JdiValue(_, thread))
      .recoverWith(wrapInvocationException(thread))
}

object JdiClass {
  def apply(classType: Type, thread: ThreadReference): JdiClass =
    new JdiClass(classType.asInstanceOf[ClassType], thread)

  def apply(classObject: Value, thread: ThreadReference): JdiClass =
    JdiClass(classObject.asInstanceOf[ClassObjectReference].reflectedType, thread)
}
