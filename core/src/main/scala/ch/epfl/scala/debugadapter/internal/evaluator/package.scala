package ch.epfl.scala.debugadapter.internal

import com.sun.jdi._

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

package object evaluator {
  private[evaluator] def method(name: String, referenceType: ReferenceType) =
    Try(referenceType.methodsByName(name).asScala.headOption).toOption.flatten

  private[evaluator] def method(name: String, signature: String, referenceType: ReferenceType) =
    Try(referenceType.methodsByName(name, signature).asScala.headOption).toOption.flatten

  private[evaluator] def invokeMethod(objRef: ObjectReference, method: Method, args: List[Value], thread: ThreadReference) =
    Try(objRef.invokeMethod(thread, method, args.asJava, ObjectReference.INVOKE_SINGLE_THREADED)) match {
      case Failure(error: InvocationException) =>
        println(error.exception())
        None
      case Success(result) => Some(result)
    }

  private[evaluator] def classLoader(objRef: ObjectReference): Option[ClassLoaderReference] =
    Try(objRef.referenceType().classLoader()).toOption
}