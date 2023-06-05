package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import scala.meta.{Type => _, _}

class RuntimeEvaluation private (
    evaluator: RuntimeEvaluator,
    validator: RuntimeValidator,
    logger: Logger
) {
  def validate(expression: String): Validation[RuntimeEvaluableTree] =
    validator.validate(expression)

  def evaluate(expression: RuntimeEvaluableTree): Safe[JdiValue] =
    evaluator.evaluate(expression)
}

object RuntimeEvaluation {
  def apply(
      frame: JdiFrame,
      logger: Logger,
      evaluator: RuntimeEvaluator,
      validator: RuntimeValidator
  ): RuntimeEvaluation =
    new RuntimeEvaluation(
      evaluator,
      validator,
      logger
    )
}

trait RuntimeValidator {
  def frame: JdiFrame
  def logger: Logger

  def validate(expression: String): Validation[RuntimeEvaluableTree]

  def validate(expression: Stat): Validation[RuntimeEvaluableTree]

  /**
   * Validates an expression, with access to class lookup.
   *
   * Because it might return a [[ClassTree]], its result might not be evaluable and must be contained in an [[EvaluationTree]]
   *
   * @param expression
   * @return a [[ValidationTree]] of the expression
   */
  protected def validateWithClass(expression: Stat): Validation[RuntimeTree]

  def validateLiteral(lit: Lit): Validation[LiteralTree]

  def localVarTreeByName(name: String): Validation[RuntimeEvaluableTree]

  /**
   * Returns a [[FieldTree]] if the name is a field, or a [[ModuleTree]] if it is a module (in Scala 3 we can access modules nested in modules by a field)
   *
   * It must load the field type if it isn't already loaded.
   *
   * It should convert the tree to a [[StaticFieldTree]] if the field is static.
   *
   * @param of
   * @param name
   * @return a [[RuntimeEvaluableTree]] representing the field or module
   */
  def fieldTreeByName(of: Validation[RuntimeTree], name: String): Validation[RuntimeEvaluableTree]

  /**
   * Returns a [[MethodTree]] if the name is a 0-arg method.
   *
   * It must load the method return type if it isn't already loaded.
   *
   * It should convert the tree to a [[StaticMethodTree]] if the method is static.
   *
   * @param of
   * @param name
   * @return
   */
  def zeroArgMethodTreeByName(
      of: Validation[RuntimeTree],
      name: String
  ): Validation[RuntimeEvaluableTree]

  /**
   * Returns a [[ModuleTree]], if the name is a (nested) module
   *
   * If it is a nested, then a check is performed to ensure that the module is not accessed from a static context (e.g. when $of is a [[ClassTree]])
   *
   * @param name
   * @param of an option representing the qualifier of the module. It helps resolving name conflicts by selecting the most suitable one
   * @return a [[ModuleTree]] representing the module
   */
  def validateModule(name: String, of: Option[RuntimeTree]): Validation[RuntimeEvaluableTree]

  /**
   * Returns a [[ClassTree]]] if the name is a (nested) class. Fails when accessing a non-static class from a static context (e.g. when of is a [[ClassTree]])
   *
   * @param name
   * @param of the potential parent of the class, can be another [[ClassTree]]
   * @return a [[ClassTree]] representing the class
   */
  def validateClass(name: String, of: Option[RuntimeTree]): Validation[ClassTree]

  def validateName(value: Term.Name, of: Validation[RuntimeTree]): Validation[RuntimeEvaluableTree]

  /**
   * Find the apply method on the given module, with the given arguments
   *
   * @param moduleName
   * @param on qualifier of the module, might help resolving name conflicts
   * @param args
   * @return
   */
  def validateApplyCall(
      moduleName: String,
      on: RuntimeTree,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[MethodTree]

  /**
   * "Unwrap" the apply method hidden by a 0-arg method returning the module
   *
   * @param on the tree on which the 0-arg method is called
   * @param name the name of the 0-arg method
   * @param args the argument of the apply method
   * @return
   */
  def validateImplicitApplyCall(
      on: RuntimeTree,
      name: String,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[MethodTree]

  /**
   * Returns a [[MethodTree]] representing the method which name and arguments type match the given ones
   *
   * If none are found, look for apply calls (explicit or implicit)
   *
   * @param tree
   * @param name
   * @param args
   * @return a [[MethodTree]] representing the method
   */
  def findMethod(
      tree: RuntimeTree,
      name: String,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[MethodTree]

  /**
   * @param call the standardize call
   * @return a [[PrimitiveBinaryOpTree]] or [[PrimitiveUnaryOpTree]] if the method is primitive. Otherwise, returns a [[MethodTree]] representing the call
   */
  def validateMethod(call: Call): Validation[RuntimeEvaluableTree]

  def getSelectedTerm(of: RuntimeTree, name: String): Validation[RuntimeEvaluableTree]

  def validateSelect(select: Term.Select): Validation[RuntimeEvaluableTree]

  def validateNew(newValue: Term.New): Validation[RuntimeEvaluableTree]
}

trait RuntimeEvaluator {
  def frame: JdiFrame
  def logger: Logger

  /**
   * Evaluates an expression. Recursively evaluates its sub-expressions
   *
   * The debugger encapsulate variables in references. The returned value must be dereferenced to get the actual value
   *
   * @param stat
   * @return a [[Safe[JdiValue]]] of the result of the evaluation
   */
  def evaluate(stat: RuntimeEvaluableTree): Safe[JdiValue]

  def evaluateLiteral(tree: LiteralTree): Safe[JdiValue]

  def evaluateOuter(tree: OuterTree): Safe[JdiValue]

  def evaluateField(tree: InstanceFieldTree): Safe[JdiValue]

  def evaluateStaticField(tree: StaticFieldTree): Safe[JdiValue]

  def invokeStatic(tree: StaticMethodTree): Safe[JdiValue]

  def invokePrimitive(tree: PrimitiveBinaryOpTree): Safe[JdiValue]

  def invokePrimitive(tree: PrimitiveUnaryOpTree): Safe[JdiValue]

  def invoke(tree: InstanceMethodTree): Safe[JdiValue]

  def evaluateModule(tree: ModuleTree): Safe[JdiValue]

  def instantiate(tree: NewInstanceTree): Safe[JdiObject]
}
