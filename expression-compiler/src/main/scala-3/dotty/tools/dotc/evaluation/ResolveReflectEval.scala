package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.transform.SymUtils.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.NameKinds.QualifiedInfo

class ResolveReflectEval(using evalCtx: EvaluationContext) extends MiniPhase:
  override def phaseName: String = ResolveReflectEval.name

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    if tree.symbol == evalCtx.evaluationClass
    then ExpressionTransformer.transform(tree)
    else tree

  object ExpressionTransformer extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case tree: Apply if isReflectEval(tree.fun.symbol) =>
          val qualifier :: _ :: args :: Nil = tree.args.map(transform)
          tree.fun.attachment(EvaluationStrategy) match
            case EvaluationStrategy.This => getLocalValue("$this")
            case EvaluationStrategy.Outer => getField(qualifier, "$outer")
            case EvaluationStrategy.LocalValue(value) =>
              getLocalValue(value.name.toString)
            case EvaluationStrategy.ClassCapture(value, cls) =>
              getClassCapture(qualifier, value.name, cls)
                .getOrElse(
                  throw new Exception(s"No capture found for $value in $cls")
                )
            case EvaluationStrategy.MethodCapture(value, method) =>
              getMethodCapture(method, value.name)
                .getOrElse(
                  throw new Exception(s"No capture found for $value in $method")
                )
            case EvaluationStrategy.StaticObject(obj) => getStaticObject(obj)
            case EvaluationStrategy.Field(field) =>
              getField(qualifier, field.name.toString)
            case EvaluationStrategy.MethodCall(method) =>
              callMethod(qualifier, method, args)
            case EvaluationStrategy.ConstructorCall(ctr, cls) =>
              callConstructor(qualifier, ctr, args)
        case _ => super.transform(tree)

  private def isReflectEval(symbol: Symbol)(using Context): Boolean =
    symbol.name == termName(
      "reflectEval"
    ) && symbol.owner == evalCtx.evaluationClass

  private def getLocalValue(name: String)(using Context): Tree =
    Apply(
      Select(This(evalCtx.evaluationClass), termName("getLocalValue")),
      List(Literal(Constant(name)))
    )

  private def getClassCapture(
      qualifier: Tree,
      originalName: Name,
      cls: ClassSymbol
  )(using Context): Option[Tree] =
    cls.info.decls.iterator
      .filter(term => term.isField)
      .find { field =>
        field.name match
          case DerivedName(underlying, _) if field.isPrivate =>
            underlying == originalName
          case DerivedName(DerivedName(_, info: QualifiedInfo), _) =>
            info.name == originalName
          case _ => false
      }
      .map(field => getField(qualifier, field.name.toString))

  private def getMethodCapture(method: TermSymbol, originalName: TermName)(using
      Context
  ): Option[Tree] =
    val methodType = method.info.asInstanceOf[MethodType]
    methodType.paramNames
      .find {
        case DerivedName(n, _) => n == originalName
        case _ => false
      }
      .map(param => getLocalValue(param.toString))

  private def getStaticObject(obj: ClassSymbol)(using Context): Tree =
    val className = JavaEncoding.encode(obj)
    Apply(
      Select(This(evalCtx.evaluationClass), termName("getStaticObject")),
      List(Literal(Constant(className)))
    )

  private def getField(qualifier: Tree, field: String)(using Context): Tree =
    Apply(
      Select(This(evalCtx.evaluationClass), termName("getField")),
      List(
        qualifier,
        Literal(Constant(field))
      )
    )

  private def callMethod(
      qualifier: Tree,
      method: TermSymbol,
      argsArray: Tree
  )(using Context): Tree =
    val methodType = method.info.asInstanceOf[MethodType]
    val paramTypesNames = methodType.paramInfos.map(JavaEncoding.encode)
    val paramTypesArray = JavaSeqLiteral(
      paramTypesNames.map(t => Literal(Constant(t))),
      TypeTree(ctx.definitions.StringType)
    )

    val args = argsArray.asInstanceOf[JavaSeqLiteral].elems
    val capturedArgs =
      methodType.paramNames.dropRight(args.size).map {
        case name @ DerivedName(underlying, _) =>
          capturedValue(method, underlying)
            .getOrElse(getLocalValue(underlying.toString))
        case name =>
          throw new Exception(
            s"Unidentified captured variable $name in $method"
          )
      }

    val returnTypeName = JavaEncoding.encode(methodType.resType)
    Apply(
      Select(This(evalCtx.evaluationClass), termName("callMethod")),
      List(
        qualifier,
        Literal(Constant(method.name.toString)),
        paramTypesArray,
        Literal(Constant(returnTypeName)),
        JavaSeqLiteral(
          capturedArgs ++ args,
          TypeTree(ctx.definitions.ObjectType)
        )
      )
    )

  private def callConstructor(
      qualifier: Tree,
      ctr: TermSymbol,
      argsArray: Tree
  )(using Context): Tree =
    val methodType = ctr.info.asInstanceOf[MethodType]
    val paramTypesNames = methodType.paramInfos.map(JavaEncoding.encode)
    val clsName = JavaEncoding.encode(methodType.resType)

    val args = argsArray.asInstanceOf[JavaSeqLiteral].elems
    val capturedArgs =
      methodType.paramNames.dropRight(args.size).map {
        case outer if outer.toString == "$outer" => qualifier
        case name @ DerivedName(
              underlying,
              _
            ) => // if derived then probably a capture
          capturedValue(ctr.owner, underlying)
            .getOrElse(
              throw new Exception(
                s"Unidentified captured variable $name in $ctr of ${ctr.owner}"
              )
            )
        case name => getLocalValue(name.toString)
      }

    val paramTypesArray = JavaSeqLiteral(
      paramTypesNames.map(t => Literal(Constant(t))),
      TypeTree(ctx.definitions.StringType)
    )
    Apply(
      Select(This(evalCtx.evaluationClass), termName("callConstructor")),
      List(
        Literal(Constant(clsName)),
        paramTypesArray,
        JavaSeqLiteral(
          capturedArgs ++ args,
          TypeTree(ctx.definitions.ObjectType)
        )
      )
    )

  private def capturedValue(sym: Symbol, originalName: TermName)(using
      Context
  ): Option[Tree] =
    if evalCtx.classOwners.contains(sym) then
      capturedByClass(sym.asClass, originalName)
    else
    // if the captured value is not a local variables
    // then it must have been captured by the outer method
    if evalCtx.localVariables.contains(originalName.toString)
    then Some(getLocalValue(originalName.toString))
    else evalCtx.capturingMethod.flatMap(getMethodCapture(_, originalName))

  private def capturedByClass(cls: ClassSymbol, originalName: TermName)(using
      Context
  ): Option[Tree] =
    // should call getClassCapture instead
    val deepness = evalCtx.classOwners.takeWhile(_ != cls).size
    val qualifier = 0
      .until(deepness)
      .foldLeft(getLocalValue("$this"))((q, _) => getField(q, "$outer"))
    getClassCapture(qualifier, originalName, cls)

object ResolveReflectEval:
  val name = "resolve-reflect-eval"
