package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger

class RuntimeEvaluation(frame: JdiFrame, logger: Logger) {
  def evaluate(stat: RuntimeEvaluableTree): Safe[JdiValue] =
    eval(stat).map(_.derefIfRef)

  private def eval(stat: RuntimeEvaluableTree): Safe[JdiValue] =
    stat match {
      case RuntimeValueTree(value, _) => value
      case LocalVarTree(varName, _) => Safe.successful(frame.variableByName(varName).map(frame.variableValue).get)
      case primitive: BinaryOpTree => invokePrimitive(primitive)
      case primitive: UnaryOpTree => invokePrimitive(primitive)
      case module: ModuleTree => evaluateModule(module)
      case ThisTree(obj) => Safe(JdiValue(obj.instances(1).get(0), frame.thread))
      case field: InstanceFieldTree => evaluateField(field)
      case staticField: StaticFieldTree => evaluateStaticField(staticField)
      case instance: NewInstanceTree => instantiate(instance)
      case method: InstanceMethodTree => invoke(method)
      case array: ArrayElemTree => evaluateArrayElement(array)
      case branching: IfTree => evaluateIf(branching)
      case staticMethod: StaticMethodTree => invokeStatic(staticMethod)
      case assign: AssignTree => evaluateAssign(assign)
    }

  private def evaluateField(tree: InstanceFieldTree): Safe[JdiValue] =
    eval(tree.qual).map { value =>
      JdiValue(value.asObject.reference.getValue(tree.field), frame.thread)
    }

  private def evaluateStaticField(tree: StaticFieldTree): Safe[JdiValue] =
    Safe(JdiValue(tree.on.getValue(tree.field), frame.thread))

  private def invokeStatic(tree: StaticMethodTree): Safe[JdiValue] =
    for {
      args <- tree.args.map(eval).traverse
      loader <- frame.classLoader()
      argsBoxedIfNeeded <- loader.boxUnboxOnNeed(tree.method.argumentTypes(), args)
      result <- JdiClass(tree.on, frame.thread).invokeStatic(tree.method, argsBoxedIfNeeded)
    } yield result

  private def invokePrimitive(tree: BinaryOpTree): Safe[JdiValue] =
    for {
      lhs <- eval(tree.lhs).flatMap(_.unboxIfPrimitive)
      rhs <- eval(tree.rhs).flatMap(_.unboxIfPrimitive)
      loader <- frame.classLoader()
      result <- tree.op.evaluate(lhs, rhs, loader)
    } yield result

  private def invokePrimitive(tree: UnaryOpTree): Safe[JdiValue] =
    for {
      rhs <- eval(tree.rhs).flatMap(_.unboxIfPrimitive)
      loader <- frame.classLoader()
      result <- tree.op.evaluate(rhs, loader)
    } yield result

  private def invoke(tree: InstanceMethodTree): Safe[JdiValue] =
    for {
      qualValue <- eval(tree.qual)
      argsValues <- tree.args.map(eval).traverse
      loader <- frame.classLoader()
      argsBoxedIfNeeded <- loader.boxUnboxOnNeed(tree.method.argumentTypes(), argsValues)
      result <- qualValue.asObject.invoke(tree.method, argsBoxedIfNeeded)
    } yield result

  private def evaluateModule(tree: ModuleTree): Safe[JdiValue] =
    tree match {
      case TopLevelModuleTree(mod) => Safe(JdiObject(mod.instances(1).get(0), frame.thread))
      case NestedModuleTree(_, init) => invoke(init)
    }

  private def instantiate(tree: NewInstanceTree): Safe[JdiObject] =
    for {
      args <- tree.init.args.map(eval).traverse
      loader <- frame.classLoader()
      boxedUnboxedArgs <- loader.boxUnboxOnNeed(tree.init.method.argumentTypes(), args)
      instance <- JdiClass(tree.`type`, frame.thread).newInstance(tree.init.method, boxedUnboxedArgs)
    } yield instance

  private def evaluateArrayElement(tree: ArrayElemTree): Safe[JdiValue] =
    for {
      array <- eval(tree.array)
      index <- eval(tree.index).flatMap(_.unboxIfPrimitive).flatMap(_.toInt)
    } yield array.asArray.getValue(index)

  private def evaluateIf(tree: IfTree): Safe[JdiValue] =
    for {
      predicate <- eval(tree.p).flatMap(_.unboxIfPrimitive).flatMap(_.toBoolean)
      value <- if (predicate) eval(tree.thenp) else eval(tree.elsep)
    } yield value

  private def evaluateAssign(tree: AssignTree): Safe[JdiValue] = {
    eval(tree.rhs)
      .flatMap { rhsValue =>
        tree.lhs match {
          case field: InstanceFieldTree =>
            eval(field.qual).map { qualValue =>
              qualValue.asObject.reference.setValue(field.field, rhsValue.value)
            }
          case field: StaticFieldTree => Safe(field.on.setValue(field.field, rhsValue.value))
          case localVar: LocalVarTree =>
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
