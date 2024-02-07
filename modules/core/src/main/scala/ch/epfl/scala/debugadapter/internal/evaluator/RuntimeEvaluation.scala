package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import com.sun.jdi.ClassType
import ch.epfl.scala.debugadapter.internal.evaluator.RuntimeEvaluationTree.*

class RuntimeEvaluation(frame: JdiFrame, logger: Logger) {
  def evaluate(stat: RuntimeEvaluationTree): Safe[JdiValue] =
    eval(stat).map(_.derefIfRef)

  private def eval(stat: RuntimeEvaluationTree): Safe[JdiValue] =
    stat match {
      case Value(value, _) => value
      case LocalVar(varName, _) => Safe.successful(frame.variableByName(varName).map(frame.variableValue).get)
      case primitive: CallBinaryOp => invokePrimitive(primitive)
      case primitive: CallUnaryOp => invokePrimitive(primitive)
      case StaticModule(mod) => Safe(JdiObject(mod.instances(1).get(0), frame.thread))
      case NestedModule(_, init) => invoke(init)
      case This(obj) => Safe(JdiValue(obj.instances(1).get(0), frame.thread))
      case field: InstanceField => evaluateField(field)
      case staticField: StaticField => evaluateStaticField(staticField)
      case instance: NewInstance => instantiate(instance)
      case method: CallInstanceMethod => invoke(method)
      case array: ArrayElem => evaluateArrayElement(array)
      case branching: If => evaluateIf(branching)
      case staticMethod: CallStaticMethod => invokeStatic(staticMethod)
      case assign: Assign => evaluateAssign(assign)
    }

  private def evaluateField(tree: InstanceField): Safe[JdiValue] =
    eval(tree.qualifier).map(_.asObject.getField(tree.field))

  private def evaluateStaticField(tree: StaticField): Safe[JdiValue] =
    Safe(JdiValue(tree.field.declaringType.getValue(tree.field), frame.thread))

  private def invokeStatic(tree: CallStaticMethod): Safe[JdiValue] =
    for {
      args <- tree.args.map(eval).traverse
      loader <- frame.classLoader()
      argsBoxedIfNeeded <- loader.boxUnboxOnNeed(tree.method.argumentTypes(), args)
      result <- JdiClass(tree.qualifier, frame.thread).invokeStatic(tree.method, argsBoxedIfNeeded)
    } yield result

  private def invokePrimitive(tree: CallBinaryOp): Safe[JdiValue] =
    for {
      lhs <- eval(tree.lhs).flatMap(_.unboxIfPrimitive)
      rhs <- eval(tree.rhs).flatMap(_.unboxIfPrimitive)
      loader <- frame.classLoader()
      result <- tree.op.evaluate(lhs, rhs, loader)
    } yield result

  private def invokePrimitive(tree: CallUnaryOp): Safe[JdiValue] =
    for {
      rhs <- eval(tree.rhs).flatMap(_.unboxIfPrimitive)
      loader <- frame.classLoader()
      result <- tree.op.evaluate(rhs, loader)
    } yield result

  private def invoke(tree: CallInstanceMethod): Safe[JdiValue] =
    for {
      qualValue <- eval(tree.qualifier)
      argsValues <- tree.args.map(eval).traverse
      loader <- frame.classLoader()
      argsBoxedIfNeeded <- loader.boxUnboxOnNeed(tree.method.argumentTypes(), argsValues)
      result <- qualValue.asObject.invoke(tree.method, argsBoxedIfNeeded)
    } yield result

  private def instantiate(tree: NewInstance): Safe[JdiObject] =
    for {
      args <- tree.init.args.map(eval).traverse
      loader <- frame.classLoader()
      boxedUnboxedArgs <- loader.boxUnboxOnNeed(tree.init.method.argumentTypes(), args)
      instance <- JdiClass(tree.`type`, frame.thread).newInstance(tree.init.method, boxedUnboxedArgs)
    } yield instance

  private def evaluateArrayElement(tree: ArrayElem): Safe[JdiValue] =
    for {
      array <- eval(tree.array)
      index <- eval(tree.index).flatMap(_.unboxIfPrimitive).flatMap(_.toInt)
      value <- array.asArray.getValue(index)
    } yield value

  private def evaluateIf(tree: If): Safe[JdiValue] =
    for {
      predicate <- eval(tree.p).flatMap(_.unboxIfPrimitive).flatMap(_.toBoolean)
      value <- if (predicate) eval(tree.thenp) else eval(tree.elsep)
    } yield value

  private def evaluateAssign(tree: Assign): Safe[JdiValue] = {
    eval(tree.rhs)
      .flatMap { rhsValue =>
        tree.lhs match {
          case InstanceField(field, qualifier) =>
            eval(qualifier).map { qualValue =>
              qualValue.asObject.reference.setValue(field, rhsValue.value)
            }
          case StaticField(field) =>
            Safe(field.declaringType.asInstanceOf[ClassType].setValue(field, rhsValue.value))
          case localVar: LocalVar =>
            val localVarRef = frame.variableByName(localVar.name)
            Safe(localVarRef.map(frame.setVariable(_, rhsValue)).get)
        }
      }
      .map(_ => JdiValue(frame.thread.virtualMachine.mirrorOfVoid, frame.thread))
  }
}

object RuntimeEvaluation {
  def apply(frame: JdiFrame, logger: Logger): RuntimeEvaluation =
    new RuntimeEvaluation(frame, logger)
}
