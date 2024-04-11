package scala.tools.nsc.evaluation

import scala.tools.nsc.Global
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.Settings

private[nsc] class ExpressionGlobal(
    settings: Settings,
    reporter: Reporter,
    uniqueName: String,
    val breakpointLine: Int,
    val expression: String,
    val localVariables: Set[String]
) extends Global(settings, reporter) {
  override protected def computeInternalPhases(): Unit = {
    super.computeInternalPhases()

    addToPhasesSet(
      new InsertExpression(this),
      "Insert expression which is going to be evaluated"
    )
    addToPhasesSet(
      new GenerateExpression(this),
      "Generate the final form of the expression"
    )
  }

  val expressionTermName: TermName = TermName(uniqueName.toLowerCase.toString)
  val expressionClassName: TypeName = TypeName(uniqueName)

  var pckg: String = null
  var expressionSymbol: TermSymbol = null
  // all classes and def in the chain of owners of the expression from local to global
  // we store them to resolve the captured variables
  var classOwners: Seq[ClassSymbol] = null
  var capturingMethod: Option[TermSymbol] = None

  def store(exprSym: Symbol): Unit = {
    expressionSymbol = exprSym.asTerm
    classOwners = exprSym.ownersIterator.collect { case cls: ClassSymbol => cls }.toSeq
    // TODO: Add test from logicallyEnclosingMember scaladoc
    capturingMethod = exprSym.ownersIterator
      .find(sym => (sym.isClass || sym.isMethod) && sym.logicallyEnclosingMember.isMethod) // the first local class or method
      .collect { case sym if sym.isMethod => sym.asTerm } // if it is a method
  }

  def expressionClass: ClassSymbol =
    if (pckg.isEmpty) rootMirror.getRequiredClass(expressionClassName.toString)
    else rootMirror.getRequiredClass(s"$pckg.$expressionClassName")

  def evaluateMethod: Symbol =
    expressionClass.info.decl(TermName("evaluate"))

  case object ExpressionAttachment extends PlainAttachment

  /**
   * The [[ExtractExpression]] phase attaches an [[EvaluationStrategy]] to each `reflectEval` node
   * to store information about the term that must be evaluated
   * Later, the [[ResolveReflectEval]] phase transforms each evaluation strategy into a call of
   * a method of the evaluation class.
   */
  sealed trait EvaluationStrategy

  object EvaluationStrategy extends PlainAttachment {
    case class This(cls: ClassSymbol) extends EvaluationStrategy
    case class Outer(outerCls: ClassSymbol)  extends EvaluationStrategy
    // the $outer param in a constructor
    case class LocalOuter(outerCls: ClassSymbol) extends EvaluationStrategy
    case class LocalValue(variable: TermSymbol, isByName: Boolean) extends EvaluationStrategy
    case class LocalValueAssign(variable: TermSymbol) extends EvaluationStrategy
    case class MethodCapture(variable: TermSymbol, method: TermSymbol, isByName: Boolean) extends EvaluationStrategy
    case class MethodCaptureAssign(variable: TermSymbol, method: TermSymbol) extends EvaluationStrategy
    case class ClassCapture(variable: TermSymbol, cls: ClassSymbol, isByName: Boolean) extends EvaluationStrategy
    case class ClassCaptureAssign(variable: TermSymbol, cls: ClassSymbol) extends EvaluationStrategy
    case class StaticObject(obj: ClassSymbol) extends EvaluationStrategy
    case class Field(field: TermSymbol, isByName: Boolean) extends EvaluationStrategy
    case class FieldAssign(field: TermSymbol) extends EvaluationStrategy
    case class MethodCall(method: TermSymbol) extends EvaluationStrategy
    case class ConstructorCall(ctr: TermSymbol, cls: ClassSymbol) extends EvaluationStrategy
  }
}
