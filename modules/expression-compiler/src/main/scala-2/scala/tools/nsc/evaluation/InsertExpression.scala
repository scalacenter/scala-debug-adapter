package scala.tools.nsc.evaluation

import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.internal.util.SourceFile
import scala.reflect.io.VirtualFile
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.transform.TypingTransformers

class InsertExpression(override val global: ExpressionGlobal) extends Transform with TypingTransformers {
  import global._

  override val phaseName: String = "insert-expression"
  override val runsAfter: List[String] = List("parser")
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
        |    unwrapException(method.invoke(obj, args: _*))
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
        |  def setField(obj: Any, className: String, fieldName: String, value: Any): Unit = {
        |    val clazz = classLoader.loadClass(className)
        |    val field = clazz.getDeclaredField(fieldName)
        |    field.setAccessible(true)
        |    field.set(obj, value)
        |  }
        |
        |  def getOuter(obj: Any, outerTypeName: String): Any = {
        |    val clazz = obj.getClass
        |    val field = getSuperclassIterator(clazz)
        |      .flatMap(_.getDeclaredFields.toSeq)
        |      .find { field => field.getName == "$$outer" && field.getType.getName == outerTypeName }
        |      .getOrElse(throw new NoSuchFieldException("$$outer"))
        |    field.setAccessible(true)
        |    field.get(obj)
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
        |    Iterator.iterate[Class[_]](clazz)(_.getSuperclass)
        |
        |  // A fake method that is used as a placeholder in the extract-expression phase.
        |  // The resolve-reflect-eval phase resolves it to a call of one of the other methods in this class.
        |  def reflectEval(qualifier: Any, term: String, args: Array[Any]): Any = ???
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
    private var expressionInserted = false

    private def filterOutTailRec(defdef: DefDef): DefDef = {
      val copiedMods = defdef.mods.copy(
        annotations = defdef.mods.annotations.filterNot {
          case Apply(
                Select(
                  New(Select(Select(Ident(scala), annotation), tailrec)),
                  _
                ),
                List()
              ) =>
            scala.toString == "scala" &&
            annotation.toString() == "annotation" &&
            tailrec.toString() == "tailrec"
          case _ => false
        }
      )
      treeCopy.DefDef(
        defdef,
        copiedMods,
        defdef.name,
        defdef.tparams,
        defdef.vparamss,
        defdef.tpt,
        defdef.rhs
      )
    }

    override def transform(tree: Tree): Tree = tree match {
      case tree: DefDef if tree.pos.line == breakpointLine =>
        insertAt(tree.pos)(
          filterOutTailRec(
            treeCopy.DefDef(
              tree,
              tree.mods,
              tree.name,
              tree.tparams,
              tree.vparamss,
              tree.tpt,
              mkExprBlock(tree.rhs)
            )
          )
        )
      case tree: DefDef =>
        super.transform(filterOutTailRec(tree))
      case vd: ValDef if vd.pos.line == breakpointLine =>
        insertAt(vd.pos)(
          treeCopy.ValDef(
            vd,
            vd.mods,
            vd.name,
            vd.tpt,
            mkExprBlock(vd.rhs)
          )
        )
      case tree if tree.pos.line == breakpointLine =>
        insertAt(tree.pos)(mkExprBlock(tree))
      case tree: PackageDef =>
        val transformed = super.transform(tree).asInstanceOf[PackageDef]
        if (expressionInserted) {
          expressionInserted = false
          atPos(tree.pos)(
            treeCopy.PackageDef(
              transformed,
              transformed.pid,
              transformed.stats ++ expressionClass
            )
          )
        } else {
          transformed
        }
      case _ =>
        super.transform(tree)
    }

    private def insertAt(pos: Position)(tree: Tree): Tree = {
      expressionInserted = true
      atPos(pos)(tree)
    }

    private def mkExprBlock(tree: Tree): Tree = {
      val block =
        if (tree.isDef)
          Block(List(expression, tree), Literal(Constant(())))
        else
          Block(List(expression), tree)
      addExpressionAttachment(block)
    }

    // `ExpressionAttachment` allows to find the inserted expression later on
    private def addExpressionAttachment(tree: Tree): Tree = {
      val attachments = tree.attachments.update(ExpressionAttachment)
      tree.setAttachments(attachments)
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
      new BatchSourceFile(expressionFile.file, wrappedExpression.toCharArray) {
        override def positionInUltimateSource(pos: Position) =
          if (!pos.isDefined) super.positionInUltimateSource(pos)
          else pos.withSource(expressionFile).withShift(-prefix.size)
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
}
