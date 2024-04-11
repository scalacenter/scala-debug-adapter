package dotty.tools.dotc

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.evaluation.SymUtils.*

class ExpressionContext(
    uniqueName: String,
    val breakpointLine: Int,
    val expression: String,
    val localVariables: Set[String],
    val pckg: String,
    val testMode: Boolean
):
  val expressionTermName: TermName = termName(uniqueName.toLowerCase.toString)
  val expressionClassName: TypeName = typeName(uniqueName)

  var expressionSymbol: TermSymbol = null
  // all classes and def in the chain of owners of the expression from local to global
  // we store them to resolve the captured variables
  var classOwners: Seq[ClassSymbol] = null
  var capturingMethod: Option[TermSymbol] = None

  def store(exprSym: Symbol)(using Context): Unit =
    expressionSymbol = exprSym.asTerm
    classOwners = exprSym.ownersIterator.collect { case cls: ClassSymbol => cls }.toSeq
    capturingMethod = exprSym.ownersIterator
      .find(sym => (sym.isClass || sym.is(Method)) && sym.enclosure.is(Method)) // the first local class or method
      .collect { case sym if sym.is(Method) => sym.asTerm } // if it is a method

  def expressionClass(using Context): ClassSymbol =
    if pckg.isEmpty then requiredClass(expressionClassName)
    else requiredClass(s"$pckg.$expressionClassName")

  def evaluateMethod(using Context): Symbol =
    expressionClass.info.decl(termName("evaluate")).symbol
