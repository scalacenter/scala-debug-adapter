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

  /**
   * Validates an expression taken as a String.
   *
   * Parse, then recursively validates its sub-expressions
   *
   * @param expression
   * @return
   */
  def validate(expression: String): Validation[RuntimeEvaluableTree]

  /**
   * Validates an expression received as an AST. Recursively validates its sub-expressions
   *
   * @param expression
   * @return a [[Valid[RuntimeEvaluableTree]]] if the expression is correct, an [[Invalid]] otherwise
   */
  def validate(expression: Stat): Validation[RuntimeEvaluableTree]

  /**
   * Validates an expression, with access to class lookup. Its result must be contained in an [[EvaluationTree]]
   *
   * @param expression
   * @return a [[ValidationTree]] of the expression
   */
  protected def validateWithClass(expression: Stat): Validation[RuntimeTree]

  /**
   * Returns a ValidationTree of a [[Term.Select]] nested in another [[Term.Select]]. Provides access to [[ClassTree]], so it mustn't be used directly and must be wrapped in a [[RuntimeEvaluableTree]]
   *
   * @param select
   * @return a [[ValidationTree]] of the qualifier
   */
  protected def validateInnerSelect(select: Term.Select): Validation[RuntimeTree]

  /**
   * @param lit
   * @return a [[Valid[LiteralTree]]] if the literal is valid, [[Fatal]] otherwise
   */
  def validateLiteral(lit: Lit): Validation[LiteralTree]

  /**
   * @param name
   * @return a [[Valid[LocalVarTree]]] if name is a local variable, [[Invalid]] otherwise
   */
  def localVarTreeByName(name: String): Validation[RuntimeEvaluableTree]

  /**
   * Returns a [[FieldTree]] if name is a field, or a [[ModuleTree]] if it is a module (in Scala 3 we can access inner modules by a field)
   *
   * It must load the field type if it isn't already loaded.
   *
   * It should convert the tree to a [[StaticFieldTree]] if the field is static.
   *
   * @param of
   * @param name
   * @return a [[RuntimeEvaluationTree]] representing the field or module
   */
  def fieldTreeByName(of: Validation[RuntimeTree], name: String): Validation[RuntimeEvaluableTree]

  /**
   * Returns a [[MethodTree]] if name is a 0-arg method. Load the method return type on need.
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
   * Returns a [[ModuleTree]], if name is a (nested) module
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
   * Find the apply method on the given module, and validate the arguments
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
   * @param ref the reference type to which the 0-arg method belongs
   * @param on the tree on which is called the 0-arg method
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
   * @return Returns a [[PrimitiveBinaryOpTree]] or [[PrimitiveUnaryOpTree]] if the method is primitive. Otherwise, returns a [[MethodTree]] representing the call
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
