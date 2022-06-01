package dotty.tools.dotc.evaluation

import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.util.Property.*

/**
 * The [[ExtractExpression]] phase attaches an [[EvaluationStrategy]] to each `reflectEval` node
 * to store information about the term that must be evaluated
 * Later, the [[ResolveReflectEval]] phase transforms each evaluation strategy into a call of
 * a method of the evaluation class.
 */
enum EvaluationStrategy:
  case This
  case Outer
  case LocalValue(value: TermSymbol)
  case MethodCapture(value: TermSymbol, method: TermSymbol)
  case ClassCapture(value: TermSymbol, cls: ClassSymbol)
  case StaticObject(obj: ClassSymbol)
  case Field(field: TermSymbol)
  case MethodCall(method: TermSymbol)
  case ConstructorCall(ctr: TermSymbol, cls: ClassSymbol)

  override def toString(): String = this match
    case This => "this"
    case Outer => "outer"
    case MethodCapture(value, _) => value.toString
    case ClassCapture(value, _) => value.toString
    case LocalValue(value) => value.toString
    case StaticObject(obj) => obj.toString
    case Field(field) => field.toString
    case MethodCall(method) => method.toString
    case ConstructorCall(_, cls) => s"new $cls"

object EvaluationStrategy extends StickyKey[EvaluationStrategy]
