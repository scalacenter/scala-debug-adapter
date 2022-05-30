package dotty.tools.dotc

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Contexts.*

class EvaluationContext(
    val evaluationClassName: String,
    val breakpointLine: Int,
    val expression: String,
    val defNames: Set[String]
):
  val expressionTermName: TermName =
    termName(evaluationClassName.toLowerCase.toString)

  // should be local to insert-extracted
  var expressionTree: Tree = _
  var expressionType: Type = _

  var expressionSymbol: Symbol = _
  var evaluationClass: ClassSymbol = _
  var originalThis: ClassSymbol = _

  // all classes and def in the chain of owners of the expression from local to global
  // we store them to resolve the captured variables
  var classOwners: Seq[ClassSymbol] = _
  var capturingMethod: Option[TermSymbol] = None

  def evaluateMethod(using Context): Symbol =
    evaluationClass.info.decl(termName("evaluate")).symbol
