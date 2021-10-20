package scala.tools.nsc

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.transform.{Transform, TypingTransformers}

private[nsc] class EvalGlobal(
    settings: Settings,
    reporter: Reporter,
    val line: Int,
    val expression: String,
    defNames: Set[String],
    expressionClassName: String,
    valuesByNameIdentName: String,
    callPrivateMethodName: String
) extends Global(settings, reporter) {
  private val valOrDefDefs: mutable.Map[Name, ValOrDefDef] = mutable.Map()
  private val lambdas: ListBuffer[DefDef] = mutable.ListBuffer()

  private var expressionOwners: List[Symbol] = _
  private var extractedExpression: Tree = _

  private var thisSym: Symbol = _

  override protected def computeInternalPhases(): Unit = {
    super.computeInternalPhases()

    addToPhasesSet(
      new InsertExpression,
      "Insert expression which is going to be evaluated"
    )
    addToPhasesSet(
      new GenerateExpression,
      "Generate the final form of the expression"
    )
  }

  class InsertExpression extends Transform with TypingTransformers {
    override val global: EvalGlobal.this.type = EvalGlobal.this
    override val phaseName: String = "insertexpression"
    override val runsAfter: List[String] = List("parser")
    override val runsRightAfter: Option[String] = None

    override protected def newTransformer(
        unit: CompilationUnit
    ): Transformer = {
      if (unit.source.file.name == "<source>") new InsExprTransformer()
      else noopTransformer
    }

    /**
     * This transformer:
     * - inserts the expression at the line of the breakpoint, which allows us to extract all needed defs, including implicit ones too.
     * Thanks to that, evaluation of expression is done in the context of a breakpoint and logic behaves the same way like the
     * normal code would behave.
     * - inserts the expression class in the same package as the expression. This allows us to call package private methods without any trouble.
     */
    class InsExprTransformer extends Transformer {
      private val expressionClassSource =
        s"""class $expressionClassName(names: Array[String], values: Array[Object]) {
           |  val $valuesByNameIdentName = names.map(_.asInstanceOf[String]).zip(values).toMap
           |
           |  def evaluate() = {
           |    ()
           |  }
           |}
           |
           |object $expressionClassName {
           |  def $callPrivateMethodName(obj: Any, methodName: String, paramTypeNames: Array[Object], args: Array[Object]) = {
           |    val expectedParamTypeNames = paramTypeNames.map(_.asInstanceOf[String])
           |    val parameterTypes = args.map(_.getClass)
           |    val method = obj
           |      .getClass()
           |      .getDeclaredMethods()
           |      .filter(_.getName() == methodName)
           |      .find(method => {
           |        val paramTypeNames = method.getParameterTypes().map(_.getName())
           |        val paramTypeNamesMatch = expectedParamTypeNames
           |          .zip(paramTypeNames)
           |          .forall {
           |            case (expectedParamTypeName, paramTypeName) =>
           |              expectedParamTypeName == paramTypeName
           |          }
           |        method.getParameterTypes.size == paramTypeNames.size && paramTypeNamesMatch
           |      })
           |      .get
           |    method.setAccessible(true)
           |    method.invoke(obj, args: _*)
           |  }
           |}
           |""".stripMargin

      private val parsedExpression = parseExpression(expression)
      private val parsedExpressionClassAndObject =
        parseExpressionClassAndObject(
          expressionClassSource
        )

      private var expressionInserted = false

      private def parseExpression(expression: String): Tree = {
        // It needs to be wrapped because it's not possible to parse single expression.
        // `$expression` is wrapped in a block to support multiline expressions.
        val wrappedExpressionSource =
          s"""object Expression {
             |  {
             |    $expression
             |  }
             |}
             |""".stripMargin
        val parsedWrappedExpression =
          parse("<wrapped-expression>", wrappedExpressionSource)
            .asInstanceOf[PackageDef]
        val parsed = parsedWrappedExpression.stats.head
          .asInstanceOf[ModuleDef]
          .impl
          .body
          .last
          .setPos(NoPosition)
        parsed match {
          case df: ValOrDefDef =>
            Block(df, Literal(Constant(())))
          case expr => expr
        }
      }

      private def parseExpressionClassAndObject(source: String): Seq[Tree] = {
        val parsedExpressionClass =
          parse("<expression>", source).asInstanceOf[PackageDef]
        parsedExpressionClass.stats match {
          case cls :: obj :: _ =>
            cls.setPos(NoPosition)
            obj.setPos(NoPosition)
            Seq(cls, obj)
          case stats =>
            throw new IllegalArgumentException(
              s"Expected at least two statements but got ${stats.size}"
            )
        }
      }

      private def parse(sourceName: String, source: String): Tree = {
        newUnitParser(
          new CompilationUnit(new BatchSourceFile(sourceName, source))
        ).parse()
      }

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
        case tree: DefDef if tree.pos.line == line =>
          expressionInserted = true
          atPos(tree.pos)(
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
        case vd: ValDef if vd.pos.line == line =>
          expressionInserted = true
          atPos(vd.pos)(
            treeCopy.ValDef(
              vd,
              vd.mods,
              vd.name,
              vd.tpt,
              mkExprBlock(vd.rhs)
            )
          )

        case tree if tree.pos.line == line =>
          expressionInserted = true
          atPos(tree.pos)(mkExprBlock(tree))
        case tree: PackageDef =>
          val transformed = super.transform(tree).asInstanceOf[PackageDef]
          if (expressionInserted) {
            expressionInserted = false
            atPos(tree.pos)(
              treeCopy.PackageDef(
                transformed,
                transformed.pid,
                transformed.stats ++ parsedExpressionClassAndObject
              )
            )
          } else {
            transformed
          }
        case _ =>
          super.transform(tree)
      }

      private def mkExprBlock(tree: Tree): Tree =
        if (tree.isDef)
          Block(List(parsedExpression, tree), Literal(Constant(())))
        else
          Block(List(parsedExpression), tree)
    }
  }

  /**
   * This transformer extracts transformed expression, extracts all defs (local variables, fields, arguments, etc.)
   * and transforms `Expression` class that was inserted by the [[InsertExpression]] in the following way:
   * - creates local variables that are equivalent to accessible values,
   * - inserts extracted expression at the end of the `evaluate` method,
   * - modifies the return type of `evaluate` method.
   */
  class GenerateExpression extends Transform with TypingTransformers {
    override val global: EvalGlobal.this.type = EvalGlobal.this
    override val phaseName: String = "generateexpression"
    override val runsAfter: List[String] = List("delambdafy")
    override val runsRightAfter: Option[String] = None

    private lazy val originalThisSymbol: ClassSymbol = expressionOwners
      .find(_.isClass)
      .map(_.asInstanceOf[ClassSymbol])
      .get
    private lazy val symbolsByName: Map[Name, Symbol] =
      valOrDefDefs
        .map(valOrDefDef => valOrDefDef._1 -> valOrDefDef._2.symbol)
        .toMap + (TermName("$this") -> originalThisSymbol)

    override protected def newTransformer(
        unit: CompilationUnit
    ): Transformer = {
      if (unit.source.file.name == "<source>") new ExprEvalTransformer(unit)
      else noopTransformer
    }

    class ExprEvalTransformer(unit: CompilationUnit) extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case _ =>
          new ExpressionExtractor().traverse(tree)
          new DefExtractor().traverse(tree)
          new GenExprTransformer(unit).transform(tree)
      }
    }

    /**
     * Extracts transformed expression that was inserted by the [[InsertExpression]] at the line of the breakpoint.
     */
    class ExpressionExtractor extends Traverser {

      private def expressionExtracted = extractedExpression != null

      override def traverse(tree: Tree): Unit = tree match {
        // Don't extract expression from the Expression class
        case tree: ClassDef if tree.name.decode == expressionClassName =>
        // ignore
        case tree: DefDef if !expressionExtracted && tree.pos.line == line =>
          expressionOwners = ownerChain(tree)
          extractedExpression = extractExpression(tree.rhs)
        // default arguments will have an additional method generated, which we need to skip
        case tree: ValDef if tree.rhs.isEmpty =>
        case tree: ValDef if !expressionExtracted && tree.pos.line == line =>
          expressionOwners = ownerChain(tree)
          extractedExpression = extractExpression(tree.rhs)
        case _ if !expressionExtracted && tree.pos.line == line =>
          expressionOwners = ownerChain(tree)
          extractedExpression = extractExpression(tree)
        case _ =>
          super.traverse(tree)
      }

      private def ownerChain(tree: Tree): List[Symbol] =
        if (tree.symbol == null)
          currentOwner.ownerChain
        else
          tree.symbol.ownerChain

      private def extractExpression(tree: Tree): Tree = tree match {
        case tree: Block =>
          tree.stats.head
        case _ =>
          tree
      }
    }

    /**
     * Extracts all defs (local variables, fields, arguments, etc.) that are accessible in the line of the breakpoint.
     */
    class DefExtractor extends Traverser {
      def isCorrectOwner(tree: Tree): Boolean = {
        expressionOwners.contains(tree.symbol.owner) ||
        (tree.symbol.owner.isConstructor) && expressionOwners.contains(
          tree.symbol.owner.owner
        )
      }

      override def traverse(tree: Tree): Unit = tree match {
        case tree: ClassDef if tree.name.decode == expressionClassName =>
          thisSym = tree.symbol
        case tree: ValDef
            if isCorrectOwner(tree) && defNames.contains(tree.name.decode) =>
          valOrDefDefs += (tree.name -> tree)
          super.traverse(tree)
        case tree: DefDef
            if isCorrectOwner(tree) && tree.symbol.isGetter &&
              defNames.contains(tree.name.decode) =>
          valOrDefDefs += (tree.name -> tree)
          super.traverse(tree)
        case tree: DefDef if isCorrectOwner(tree) && isLambda(tree) =>
          lambdas += tree
          super.traverse(tree)
        case _ =>
          super.traverse(tree)
      }
    }

    /**
     * Transforms `Expression` class:
     * - for every extracted def copy its value to the local variable in the method `evaluate` in the form:
     * `val foo: String = valuesByName("foo").asInstanceOf[String]`,
     * - inserts the expression (the one that was inserted at the line of the breakpoint by [[InsertExpression]])
     * at the end of `evaluate` method,
     * - replaces symbols in the expression with their local equivalents
     * - modifies return type of the `evaluate` method.
     */
    class GenExprTransformer(unit: CompilationUnit)
        extends TypingTransformer(unit) {
      private var valuesByNameIdent: Ident = _

      override def transform(tree: Tree): Tree = tree match {
        // Discard classes different than Expression
        case tree: ClassDef if tree.name.decode != expressionClassName =>
          EmptyTree
        case tree: ClassDef if tree.name.decode == expressionClassName =>
          thisSym = tree.symbol

          val typer = localTyper.atOwner(tree, tree.symbol)
          val transformedLambdas = lambdas.map(lambda => {
            lambda.symbol.owner = thisSym
            deriveDefDef(lambda)(rhs =>
              new LambdaExpressionTransformer(
                typer
              )(unit).transform(rhs)
            )
          })

          val derivedClass = deriveClassDef(tree) { template =>
            Template(template.symbol, template.body ++ transformedLambdas)
          }
          super.transform(derivedClass)
        case tree: Ident
            if tree.name == TermName(
              valuesByNameIdentName
            ) && valuesByNameIdent == null =>
          valuesByNameIdent = tree
          EmptyTree
        case tree: DefDef if tree.name == TermName("evaluate") =>
          // firstly, transform the body of the method
          val transformed = super.transform(tree).asInstanceOf[DefDef]
          // replace symbols in the expression with those from the `evaluate` method
          val newExpression =
            new ExpressionTransformer(
              localTyper.atOwner(tree, tree.symbol)
            )(unit)
              .transform(extractedExpression)

          val typedNewExpression =
            typer
              .typedPos(transformed.pos)(newExpression)
              .setType(newExpression.tpe)

          val tpe = typedNewExpression.tpe

          val derived: DefDef = treeCopy.DefDef(
            transformed,
            transformed.mods,
            transformed.name,
            transformed.tparams,
            transformed.vparamss,
            TypeTree(tpe),
            typedNewExpression
          )

          // update return type of the `evaluate` method
          derived.symbol
            .asInstanceOf[MethodSymbol]
            .modifyInfo(info => {
              val methodType = info.asInstanceOf[MethodType]
              methodType.copy(resultType = tpe)
            })
          derived.setType(tpe)
        case _ =>
          super.transform(tree)
      }
    }

    class ExpressionTransformer(
        typer: analyzer.Typer
    )(unit: CompilationUnit)
        extends TypingTransformer(unit) {
      override def transform(tree: Tree): Tree = tree match {
        case tree: This if tree.symbol == originalThisSymbol =>
          val name = TermName("$this")
          if (defNames.contains(name.decode)) ident(name)
          else super.transform(tree)
        case tree: This =>
          tree
        case tree: Ident =>
          val name = tree.name
          if (defNames.contains(name.decode)) ident(name)
          else super.transform(tree)
        case tree: Apply if tree.fun.symbol.isGetter =>
          val fun = tree.fun.asInstanceOf[Select]
          val name = fun.name
          val qualifier = fun.qualifier
          if (qualifier.isInstanceOf[This] && symbolsByName.contains(name))
            ident(name)
          else super.transform(tree)
        case tree: Apply
            if tree.fun.isInstanceOf[Select] && tree.fun.symbol.isPrivate =>
          val privateCall = mkCallPrivate(tree)(typer)
          super.transform(privateCall)
        case _ =>
          super.transform(tree)
      }

      private def ident(name: Name) = {
        val app = Apply(
          Select(
            Apply(
              Select(This(thisSym), TermName(valuesByNameIdentName)),
              List()
            ),
            TermName("apply")
          ),
          List(Literal(Constant(name.decode)))
        )
        val symbol = symbolsByName(name)
        val tpe = symbol.tpe match {
          case tpe: MethodType =>
            tpe.resultType
          case tpe =>
            tpe
        }

        val tpt = TypeTree().setType(tpe)
        val cast = mkCast(app, tpt)
        typer.typed(cast)
      }
    }

    class LambdaExpressionTransformer(
        typer: analyzer.Typer
    )(unit: CompilationUnit)
        extends TypingTransformer(unit) {
      override def transform(tree: Tree): Tree = tree match {
        case tree: Apply
            if tree.fun.isInstanceOf[Select] && tree.fun.symbol.isPrivate =>
          super.transform(mkCallPrivate(tree)(typer))
        case _ =>
          super.transform(tree)
      }
    }

    private def mkCallPrivate(tree: Apply)(typer: analyzer.Typer) = {
      val fun = tree.fun.asInstanceOf[Select]
      val paramTypeNames = tree.args
        .map { arg =>
          val tpeSymbol = arg.tpe.typeSymbol
          val paramTypeName = if (tpeSymbol.isPrimitiveValueClass) {
            tpeSymbol.fullName.stripPrefix("scala.").toLowerCase()
          } else {
            tpeSymbol.fullName
          }
          Literal(Constant(paramTypeName))
        }
      val paramTypeNamesArray =
        ArrayValue(TypeTree(definitions.ObjectTpe), paramTypeNames)
      val argsArray = ArrayValue(TypeTree(definitions.ObjectTpe), tree.args)
      val app = Apply(
        Select(Ident(expressionClassName), TermName(callPrivateMethodName)),
        List(
          fun.qualifier,
          Literal(Constant(tree.fun.asInstanceOf[Select].name.decode)),
          paramTypeNamesArray,
          argsArray
        )
      )
      val cast = mkCast(app, TypeTree(tree.tpe))
      val typed = typer.typed(cast)
      typed
    }

    private def mkCast(tree: Tree, tpt: TypeTree) = {
      if (tpt.symbol.isPrimitiveValueClass) {
        val unboxedMethodSym =
          currentRun.runDefinitions.unboxMethod(tpt.symbol)
        Apply(unboxedMethodSym, tree)
      } else {
        val tapp = gen.mkTypeApply(
          gen
            .mkAttributedSelect(tree, definitions.Object_asInstanceOf)
            .asInstanceOf[Tree],
          List(tpt)
        )
        Apply(tapp, Nil)
      }
    }

    private def isLambda(tree: DefDef) =
      tree.symbol.isLiftedMethod && tree.symbol.name.decode
        .contains(nme.ANON_FUN_NAME)
  }
}
