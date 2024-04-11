package scala.tools.nsc.evaluation

import scala.collection.mutable
import scala.tools.nsc.transform.{Transform, TypingTransformers}

/**
 * This transformer extracts transformed expression, extracts all defs (local variables, fields, arguments, etc.)
 * and transforms `Expression` class that was inserted by the [[InsertExpression]] in the following way:
 * - creates local variables that are equivalent to accessible values,
 * - inserts extracted expression at the end of the `evaluate` method,
 * - modifies the return type of `evaluate` method.
 */
class GenerateExpression(override val global: ExpressionGlobal) extends Transform with TypingTransformers {
  import global._
  val valOrDefDefs: mutable.Map[Name, ValOrDefDef] = mutable.Map()
  val lambdas: mutable.ListBuffer[DefDef] = mutable.ListBuffer()

  var expressionOwners: List[Symbol] = _
  var extractedExpression: Tree = _

  var thisSym: Symbol = _

  override val phaseName: String = "generate-expression"
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
  ): Transformer = new ExprEvalTransformer(unit)

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
      case tree: DefDef if shouldExtract(tree) =>
        expressionOwners = ownerChain(tree)
        extractedExpression = extractExpression(tree.rhs)
      // default arguments will have an additional method generated, which we need to skip
      case tree: ValDef if tree.rhs.isEmpty =>
      case tree: ValDef if shouldExtract(tree) =>
        expressionOwners = ownerChain(tree)
        extractedExpression = extractExpression(tree.rhs)
      case _ if shouldExtract(tree) =>
        expressionOwners = ownerChain(tree)
        extractedExpression = extractExpression(tree)
      case _ =>
        super.traverse(tree)
    }

    private def shouldExtract(tree: Tree): Boolean =
      !expressionExtracted && tree.attachments
        .get[ExpressionAttachment.type]
        .isDefined

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
          if isCorrectOwner(tree) &&
            localVariables.contains(tree.name.decode) =>
        valOrDefDefs += (tree.name -> tree)
        super.traverse(tree)
      case tree: DefDef
          if isCorrectOwner(tree) && tree.symbol.isGetter &&
            localVariables.contains(tree.name.decode) =>
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
  class GenExprTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
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
          if tree.name == TermName("valuesByName") &&
            valuesByNameIdent == null =>
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
        if (localVariables.contains(name.decode)) ident(name)
        else super.transform(tree)
      case tree: This =>
        tree
      case tree: Ident =>
        val name = tree.name
        if (localVariables.contains(name.decode)) ident(name)
        else super.transform(tree)
      case tree: Apply if tree.fun.symbol.isGetter =>
        val fun = tree.fun.asInstanceOf[Select]
        val name = fun.name
        val qualifier = fun.qualifier
        if (qualifier.isInstanceOf[This] && symbolsByName.contains(name))
          ident(name)
        else super.transform(tree)
      case tree: Apply if tree.fun.isInstanceOf[Select] && tree.fun.symbol.isPrivate =>
        val privateCall = mkCallPrivate(tree)(typer)
        super.transform(privateCall)
      case _ =>
        super.transform(tree)
    }

    private def ident(name: Name) = {
      val app = Apply(
        Select(
          Apply(
            Select(This(thisSym), TermName("valuesByName")),
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

      val tpt = TypeTree(tpe)
      val cast = mkCast(app, tpt)
      typer.typed(cast)
    }
  }

  class LambdaExpressionTransformer(
      typer: analyzer.Typer
  )(unit: CompilationUnit)
      extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      case tree: Apply if tree.fun.isInstanceOf[Select] && tree.fun.symbol.isPrivate =>
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
      ArrayValue(TypeTree(definitions.StringTpe), paramTypeNames)
    val argsArray = ArrayValue(TypeTree(definitions.ObjectTpe), tree.args)
    val app = Apply(
      Select(Ident(expressionClassName), TermName("callPrivateMethod")),
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
