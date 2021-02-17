package ch.epfl.scala.debugadapter.internal

import com.microsoft.java.debug.core.IEvaluatableBreakpoint
import com.microsoft.java.debug.core.adapter.IEvaluationProvider
import com.sun.jdi.{DoubleValue, Field, FloatValue, LocalVariable, LongValue, Method, ObjectReference, PrimitiveValue, StackFrame, ThreadReference, Value, VirtualMachine}

import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters._
import scala.meta.parsers.Parsed
import scala.meta.{Stat, Term, _}
import scala.util.Try

object EvaluationProvider extends IEvaluationProvider {
  override def isInEvaluation(thread: ThreadReference): Boolean = false

  // Currently, only single expression is supported
  override def evaluate(
      expression: String,
      thread: ThreadReference,
      depth: Int
  ): CompletableFuture[Value] = {
    val frame = thread.frames().get(depth)
    val result = expression.parse[Stat] match {
      case Parsed.Success(tree) => evaluate(tree, frame, thread)
      case Parsed.Error(_, msg, _) => Left(msg)
    }
    CompletableFuture.completedFuture(result.fold(error => thread.virtualMachine().mirrorOf(error), identity))
  }

  override def evaluate(
      expression: String,
      thisContext: ObjectReference,
      thread: ThreadReference
  ): CompletableFuture[Value] = ???

  override def evaluateForBreakpoint(
      breakpoint: IEvaluatableBreakpoint,
      thread: ThreadReference
  ): CompletableFuture[Value] = ???

  override def invokeMethod(
      thisContext: ObjectReference,
      methodName: String,
      methodSignature: String,
      args: Array[Value],
      thread: ThreadReference,
      invokeSuper: Boolean
  ): CompletableFuture[Value] = ???

  override def clearState(thread: ThreadReference): Unit = {}

  private def evaluate(tree: Stat, frame: StackFrame, thread: ThreadReference): Either[String, Value] = {
    tree match {
      case _: Lit.Unit =>
        Right(frame.virtualMachine().mirrorOfVoid())
      case lit: Lit.Boolean =>
        Right(frame.virtualMachine().mirrorOf(lit.value))
      case lit: Lit.Byte =>
        Right(frame.virtualMachine().mirrorOf(lit.value))
      case lit: Lit.Char =>
        Right(frame.virtualMachine().mirrorOf(lit.value))
      case lit: Lit.Double =>
        Right(frame.virtualMachine().mirrorOf(lit.syntax.toDouble))
      case lit: Lit.Float =>
        Right(frame.virtualMachine().mirrorOf(lit.syntax.toFloat))
      case lit: Lit.Int =>
        Right(frame.virtualMachine().mirrorOf(lit.value))
      case lit: Lit.Long =>
        Right(frame.virtualMachine().mirrorOf(lit.value))
      case lit: Lit.Short =>
        Right(frame.virtualMachine().mirrorOf(lit.value))
      case lit: Lit.String =>
        Right(frame.virtualMachine().mirrorOf(lit.value))
      case term: Term.Name =>
        value(term, frame, thread).toRight(s"`${term.value}` doesn't exist")
      case Term.Select(qual, name) =>
        val result = for {
          qualVal <- evaluate(qual, frame, thread).toOption
          selected <- select(name, qualVal, thread)
        } yield selected
        result.toRight(s"`${name.value}` doesn't exist")
      case term: Term.Apply =>
        for {
          args <- evaluateAll(term.args, frame, thread)
          fun <- evaluateFun(term.fun, args, frame, thread)
          result <- invoke(fun, args, thread)
        } yield result
      case term: Term.ApplyInfix =>
        for {
          lhs <- evaluate(term.lhs, frame, thread)
          rhs <- evaluate(term.args.head, frame, thread)
          result <- evaluateOp(lhs, rhs, term.op.value, thread.virtualMachine())
        } yield result
      case _ =>
        Left("unsupported operation")
    }
  }

  private def evaluateAll(terms: List[Term], frame: StackFrame, thread: ThreadReference) =
    traverse(terms.map(term => evaluate(term, frame, thread)))

  private def evaluateFun(term: Term, args: List[Value], frame: StackFrame, thread: ThreadReference): Either[String, Fun] = term match {
    case term: Term.Name =>
      val objRef = frame.thisObject()
      methodByArgs(term, objRef, args)
        .map(method => Fun(objRef, method))
        .toRight(s"unable to invoke `${term.value}`")
    case term: Term.Select =>
      evaluate(term.qual, frame, thread) match {
        case Right(objRef: ObjectReference) =>
          methodByArgs(term.name, objRef, args)
            .map(method => Fun(objRef, method))
            .toRight(s"unable to invoke `${term.name.value}`")
        case Right(_) => Left(s"unable to invoke `${term.name.value}`")
        case Left(msg) => Left(msg)
      }
    case _ => Left("unsupported operation")
  }

  private def evaluateOp(lhs: Value, rhs: Value, op: String, vm: VirtualMachine): Either[String, Value] = (lhs, rhs) match {
    case (x: PrimitiveValue, y: PrimitiveValue) => (x, y) match {
        case (_: DoubleValue, _) | (_, _: DoubleValue) =>
          evaluateFractionals(x.doubleValue(), y.doubleValue(), op).map(vm.mirrorOf)
        case (_: FloatValue, _) | (_, _: FloatValue) =>
          evaluateFractionals(x.floatValue(), y.floatValue(), op).map(vm.mirrorOf)
        case (_: LongValue, _) | (_, _: LongValue) =>
          evaluateIntegrals(x.longValue(), y.longValue(), op).map(vm.mirrorOf)
        case (_, _) | (_, _) =>
          evaluateIntegrals(x.intValue(), y.intValue(), op).map(vm.mirrorOf)
        case _ => Left ("unsupported operation")
      }
    case _ => Left("unable to evaluate non-primitives")
  }

  private def evaluateIntegrals[T](
      x: T,
      y: T,
      op: String
  )(implicit integral: Integral[T]): Either[String, T] = op match {
    case "+" => Right(integral.plus(x, y))
    case "-" => Right(integral.minus(x, y))
    case "*" => Right(integral.times(x, y))
    case "/" => Right(integral.quot(x, y))
    case _ => Left("fail")
  }

  private def evaluateFractionals[T](
      x: T,
      y: T,
      op: String
  )(implicit fractional: Fractional[T]): Either[String, T] = op match {
    case "+" => Right(fractional.plus(x, y))
    case "-" => Right(fractional.minus(x, y))
    case "*" => Right(fractional.times(x, y))
    case "/" => Right(fractional.div(x, y))
    case _ => Left("fail")
  }

  private def invoke(fun: Fun, args: List[Value], thread: ThreadReference): Either[String, Value] =
    Try(fun.objRef.invokeMethod(thread, fun.method, args.asJava, ObjectReference.INVOKE_SINGLE_THREADED))
      .toOption
      .toRight(s"unable to invoke `${fun.method.name()}`")

  private def value(name: Term.Name, frame: StackFrame, thread: ThreadReference): Option[Value] =
    variable(name, frame)
      .flatMap(variable => variableValue(variable, frame))
      .orElse(select(name, frame.thisObject(), thread))

  private def select(name: Term.Name, value: Value, thread: ThreadReference): Option[Value] = value match {
    case objRef: ObjectReference =>
      field(name, objRef)
        .flatMap(field => fieldValue(field, objRef))
        .orElse(method(name, objRef).flatMap(method => invoke(Fun(objRef, method), List(), thread).toOption))
    case _ => None
  }

  private def variable(name: Term.Name, frame: StackFrame): Option[LocalVariable] =
    Try(frame.visibleVariableByName(name.value))
      .filter(_ != null)
      .toOption

  private def variableValue(localVariable: LocalVariable, frame: StackFrame): Option[Value] =
    Try(frame.getValue(localVariable)).toOption

  private def field(name: Term.Name, objRef: ObjectReference): Option[Field] =
    Try(objRef.referenceType().fieldByName(name.value)).toOption.filter(_ != null)

  private def fieldValue(field: Field, objRef: ObjectReference): Option[Value] =
    Try(objRef.getValue(field)).toOption

  private def method(name: Term.Name, objRef: ObjectReference): Option[Method] =
    Try(objRef.referenceType().methodsByName(name.value).asScala.headOption).toOption.flatten

  private def methodByArgs(name: Term.Name, objRef: ObjectReference, args: List[Value]): Option[Method] = {
    val argsSignature = s"(${args.map(_.`type`().signature()).mkString})"
    Try(objRef.referenceType().methodsByName(name.value).asScala.find(_.signature().startsWith(argsSignature))).toOption.flatten
  }

  private def traverse[A, B](eithers: List[Either[A, B]]): Either[A, List[B]] = {
    val builder = List.newBuilder[B]
    val it = eithers.iterator

    while (it.hasNext) it.next() match {
      case Left(a) => return Left(a)
      case Right(b) => builder += b
    }

    Right(builder.result())
  }

  case class Fun(objRef: ObjectReference, method: Method)
}
