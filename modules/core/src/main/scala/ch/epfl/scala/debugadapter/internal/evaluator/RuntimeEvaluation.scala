package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import scala.meta.{Type => _, _}

trait RuntimeValidator {
  def frame: JdiFrame
  def logger: Logger

  def validate(expression: String): Validation[RuntimeEvaluableTree]

  def validate(expression: Stat): Validation[RuntimeEvaluableTree]

  def validateBlock(block: Term.Block): Validation[RuntimeEvaluableTree]

  def validateLiteral(lit: Lit): Validation[RuntimeEvaluableTree]

  def validateName(value: String, methodFirst: Boolean): Validation[RuntimeEvaluableTree]

  def validateMethod(call: Call): Validation[RuntimeEvaluableTree]

  def validateSelect(select: Term.Select): Validation[RuntimeEvaluableTree]

  def validateNew(newValue: Term.New): Validation[RuntimeEvaluableTree]

  def validateOuter(tree: RuntimeTree): Validation[RuntimeEvaluableTree]

  def validateIf(tree: Term.If): Validation[RuntimeEvaluableTree]

  def validateAssign(tree: Term.Assign): Validation[RuntimeEvaluableTree]
}
