package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._

import scala.util.Try

class JdiClassObject(
    override val reference: ClassObjectReference,
    classLoader: JdiClassLoader,
    thread: ThreadReference
) extends JdiObject(reference, thread) {
  private val vm = thread.virtualMachine()

  def newInstance(args: List[Value]): Option[JdiObject] = {
    val parameterTypes = args.map(_.`type`().asInstanceOf[ReferenceType].classObject())
    for {
      objects <- JdiArray("java.lang.Object", args.size, classLoader, thread)
      _ <- objects.setValues(args)
      constructor <- invoke("getConstructor", parameterTypes)
        .map(_.asInstanceOf[ObjectReference])
        .map(new JdiObject(_, thread))
      instance <- constructor.invoke("newInstance", List(objects.reference))
      jdiObject <- Try(instance.asInstanceOf[ObjectReference]).toOption.map(new JdiObject(_, thread))
    } yield jdiObject
  }

  def invokeStatic(methodName: String, args: List[Value]): Option[Value] = for {
    parameterTypes <- Some(args.map(_.`type`().asInstanceOf[ReferenceType].classObject()))
    method <- invoke("getMethod", List(vm.mirrorOf(methodName)) ++ parameterTypes)
      .map(_.asInstanceOf[ObjectReference])
      .map(new JdiObject(_, thread))
    result <- method.invoke("invoke", List(null) ++ args)
  } yield result
}
