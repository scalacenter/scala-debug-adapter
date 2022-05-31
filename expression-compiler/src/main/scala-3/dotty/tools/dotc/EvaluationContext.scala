package dotty.tools.dotc

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Contexts.*

class EvaluationContext(
    uniqueName: String,
    val breakpointLine: Int,
    val expression: String,
    val defNames: Set[String],
    val pckg: String
):
  val expressionTermName: TermName = termName(uniqueName.toLowerCase.toString)
  val evaluationClassName: TypeName = typeName(uniqueName)

  // all classes and def in the chain of owners of the expression from local to global
  // we store them to resolve the captured variables
  var classOwners: Seq[ClassSymbol] = _
  var capturingMethod: Option[TermSymbol] = None

  def evaluationClass(using Context): ClassSymbol =
    if pckg.isEmpty then requiredClass(evaluationClassName)
    else requiredClass(s"$pckg.$evaluationClassName")

  def evaluateMethod(using Context): Symbol =
    evaluationClass.info.decl(termName("evaluate")).symbol
