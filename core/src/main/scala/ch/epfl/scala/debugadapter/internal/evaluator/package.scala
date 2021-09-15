package ch.epfl.scala.debugadapter.internal

import com.sun.jdi._

import scala.collection.JavaConverters._

package object evaluator {
  private[evaluator] def method(
      name: String,
      referenceType: ReferenceType
  ): Method =
    referenceType.methodsByName(name).asScala.head

  private[evaluator] def method(
      name: String,
      signature: String,
      referenceType: ReferenceType
  ): Method =
    referenceType.methodsByName(name, signature).asScala.head

  private[evaluator] def invokeMethod(
      objRef: ObjectReference,
      method: Method,
      args: List[Value],
      thread: ThreadReference
  ): Safe[Value] =
    Safe(
      objRef.invokeMethod(
        thread,
        method,
        args.asJava,
        ObjectReference.INVOKE_SINGLE_THREADED
      )
    )

  implicit class SafeList[A](seq: List[Safe[A]]) {
    def traverse: Safe[List[A]] = {
      seq.foldRight(Safe.lift(List.empty[A])) { (safeHead, safeTail) =>
        safeTail.flatMap(tail => safeHead.map(head => head :: tail))
      }
    }
  }
}
