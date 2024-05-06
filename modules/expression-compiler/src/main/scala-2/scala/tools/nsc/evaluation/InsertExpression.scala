package scala.tools.nsc.evaluation

import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.internal.util.SourceFile
import scala.reflect.io.VirtualFile
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.transform.TypingTransformers

class InsertExpression(override val global: ExpressionGlobal) extends Transform with TypingTransformers {
  import global._

  private var expressionInserted = false

  override val phaseName: String = "insert-expression"
  override val runsAfter: List[String] = List("parser")
  override val runsBefore: List[String] = List("namer")
  override val runsRightAfter: Option[String] = None

  private val evaluationClassSource =
    s"""|class $expressionClassName(thisObject: Any, names: Array[String], values: Array[Any]) {
        |  import java.lang.reflect.InvocationTargetException
        |  val classLoader = getClass.getClassLoader
        |
        |  def evaluate(): Any =
        |    ()
        |
        |  def getThisObject(): Any = thisObject
        |
        |  def getLocalValue(name: String): Any = {
        |    val idx = names.indexOf(name)
        |    if (idx == -1) throw new NoSuchElementException(name)
        |    else values(idx)
        |  }
        |
        |  def setLocalValue(name: String, value: Any): Any = {
        |    val idx = names.indexOf(name)
        |    if (idx == -1) throw new NoSuchElementException(name)
        |    else values(idx) = value
        |  }
        |
        |  def callMethod(obj: Any, className: String, methodName: String, paramTypesNames: Array[String], returnTypeName: String, args: Array[Object]): Any = {
        |    val clazz = classLoader.loadClass(className)
        |    val method = clazz.getDeclaredMethods
        |      .find { m =>
        |        m.getName == methodName &&
        |          m.getReturnType.getName == returnTypeName &&
        |          m.getParameterTypes.map(_.getName).toSeq == paramTypesNames.toSeq
        |      }
        |      .getOrElse(throw new NoSuchMethodException(methodName))
        |    method.setAccessible(true)
        |    val res = unwrapException(method.invoke(obj, args: _*))
        |    if (returnTypeName == "void") () else res
        |  }
        |
        |  def callConstructor(className: String, paramTypesNames: Array[String], args: Array[Object]): Any = {
        |    val clazz = classLoader.loadClass(className)
        |    val constructor = clazz.getConstructors
        |      .find { c => c.getParameterTypes.map(_.getName).toSeq == paramTypesNames.toSeq }
        |      .getOrElse(throw new NoSuchMethodException(s"new $$className"))
        |    constructor.setAccessible(true)
        |    unwrapException(constructor.newInstance(args: _*))
        |  }
        |
        |  def getField(obj: Any, className: String, fieldName: String): Any = {
        |    val clazz = classLoader.loadClass(className)
        |    val field = clazz.getDeclaredField(fieldName)
        |    field.setAccessible(true)
        |    field.get(obj)
        |  }
        |
        |  def setField(obj: Any, className: String, fieldName: String, value: Any): Any = {
        |    val clazz = classLoader.loadClass(className)
        |    val field = clazz.getDeclaredField(fieldName)
        |    field.setAccessible(true)
        |    field.set(obj, value)
        |  }
        |
        |  def getOuter(obj: Any, outerTypeName: String): Any = {
        |    val clazz = obj.getClass
        |    getSuperclassIterator(clazz)
        |      .flatMap(_.getDeclaredFields.toSeq)
        |      .collectFirst {
        |        case field if field.getName == "$$outer" && field.getType.getName == outerTypeName =>
        |          field.setAccessible(true)
        |          field.get(obj)
        |      }
        |      .getOrElse(null)
        |  }
        |
        |  def getStaticObject(className: String): Any = {
        |    val clazz = classLoader.loadClass(className)
        |    val field = clazz.getDeclaredField("MODULE$$")
        |    field.setAccessible(true)
        |    field.get(null)
        |  }
        |
        |  def getSuperclassIterator(clazz: Class[_]): Iterator[Class[_]] =
        |    Iterator.iterate[Class[_]](clazz)(_.getSuperclass).takeWhile(_ != null)
        |
        |  // A fake method that is used as a placeholder in the extract-expression phase.
        |  // The resolve-reflect-eval phase resolves it to a call of one of the other methods in this class.
        |  def reflectEval(qualifier: Any, term: String, args: Array[Object]): Any = ???
        |
        |  private def unwrapException(f: => Any): Any =
        |    try f catch {
        |      case e: InvocationTargetException => throw e.getCause
        |    }
        |}
        |""".stripMargin

  override protected def newTransformer(unit: CompilationUnit): Transformer =
    new Inserter(parseExpression, parseEvaluationClass)

  /**
   * This transformer:
   * - inserts the expression at the line of the breakpoint, which allows us to extract all needed defs, including implicit ones too.
   * Thanks to that, evaluation of expression is done in the context of a breakpoint and logic behaves the same way like the
   * normal code would behave.
   * - inserts the expression class in the same package as the expression. This allows us to call package private methods without any trouble.
   */
  private class Inserter(expression: Tree, expressionClass: Seq[Tree]) extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case _: PackageDef =>
        val transformed = super.transform(tree).asInstanceOf[PackageDef]
        if (expressionInserted) {
          // set to `false` to prevent inserting `Expression` class in other `PackageDef`s
          expressionInserted = false
          transformed.copy(stats = transformed.stats ++ expressionClass).copyAttrs(tree)
        } else transformed

      // should never insert an expression in a PatVarDef or a Bind
      case _: ValDef if tree.hasAttachment[PatVarDefAttachment.type] => tree
      case _: Bind => tree

      // insert expression
      case tree: DefDef if tree.rhs != EmptyTree && isOnBreakpoint(tree) =>
        tree.copy(rhs = mkExprBlock(expression, tree.rhs)).copyAttrs(tree)
      case tree: Match if isOnBreakpoint(tree) || tree.cases.exists(isOnBreakpoint) =>
        // the expression is on the match or a case of the match
        // if it is on the case of the match the program could pause on the pattern, the guard or the body
        // we assume it pauses on the pattern because that is the first instruction
        // in that case we cannot compile the expression val in the pattern, but we can compile it in the selector
        tree.copy(selector = mkExprBlock(expression, tree.selector)).copyAttrs(tree)
      case tree: ValDef if isOnBreakpoint(tree) =>
        tree.copy(rhs = mkExprBlock(expression, tree.rhs)).copyAttrs(tree)
      case _: Ident | _: Select | _: GenericApply | _: Literal | _: This | _: New | _: Assign | _: Block
          if isOnBreakpoint(tree) =>
        mkExprBlock(expression, tree)

      case tree => super.transform(tree)
    }
  }

  private def parseExpression: Tree = {
    val expressionFile = new BatchSourceFile(new VirtualFile("<expression>"), expression.toCharArray)
    val prefix =
      s"""|object Expression {
          |  {
          |    """.stripMargin
    // don't use stripMargin on wrappedExpression because expression can contain a line starting with `|`
    val wrappedExpression = prefix + expression + "\n}}\n"
    val wrappedExpressionFile =
      new BatchSourceFile(new VirtualFile("<wrapped-expression>"), wrappedExpression.toCharArray) {
        override def positionInUltimateSource(pos: Position) = {
          if (!pos.isDefined || pos.start < prefix.size || pos.end > prefix.size + expression.size)
            super.positionInUltimateSource(pos)
          else pos.withSource(expressionFile).withShift(-prefix.size)
        }
      }

    parse(wrappedExpressionFile)
      .asInstanceOf[PackageDef]
      .stats
      .head
      .asInstanceOf[ModuleDef]
      .impl
      .body
      .tail
      .head
  }

  private def parseEvaluationClass: Seq[Tree] = {
    val sourceFile = new BatchSourceFile("<evaluation class>", evaluationClassSource.toCharArray.toSeq)
    parse(sourceFile).asInstanceOf[PackageDef].stats
  }

  private def parse(sourceFile: SourceFile): Tree =
    newUnitParser(new CompilationUnit(sourceFile)).parse()

  private def isOnBreakpoint(tree: Tree): Boolean =
    tree.pos.isDefined && tree.pos.line == breakpointLine

  private def mkExprBlock(expr: Tree, tree: Tree): Tree = {
    if (expressionInserted) {
      warnOrError("expression already inserted", tree.pos)
      tree
    } else {
      expressionInserted = true
      val valDef = ValDef(Modifiers(), expressionTermName, TypeTree(), expr).setPos(tree.pos)
      Block(List(valDef), tree).setPos(tree.pos)
    }
  }

  // only fails in test mode
  private def warnOrError(msg: String, pos: Position): Unit =
    if (testMode) reporter.error(pos, msg) else reporter.warning(pos, msg)
}
