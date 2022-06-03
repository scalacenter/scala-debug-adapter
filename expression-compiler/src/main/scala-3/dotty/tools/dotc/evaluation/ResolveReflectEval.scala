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
import dotty.tools.dotc.report

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
          val qualifier :: _ :: argsTree :: Nil = tree.args.map(transform)
          val args = argsTree.asInstanceOf[JavaSeqLiteral].elems
          tree.attachment(EvaluationStrategy) match
            case EvaluationStrategy.This => getLocalValue("$this")
            case EvaluationStrategy.Outer(outerCls) =>
              getOuter(qualifier, outerCls)
            case EvaluationStrategy.LocalValue(variable) =>
              derefCapturedVar(getLocalValue(variable.name.toString), variable)
            case EvaluationStrategy.ClassCapture(variable, cls) =>
              val capture = getClassCapture(qualifier, variable.name, cls)
                .getOrElse {
                  report.error(s"No capture found for $variable in $cls")
                  ref(defn.Predef_undefined)
                }
              derefCapturedVar(capture, variable)
            case EvaluationStrategy.MethodCapture(variable, method) =>
              val capture = getMethodCapture(method, variable.name)
                .getOrElse {
                  report.error(s"No capture found for $variable in $method")
                  ref(defn.Predef_undefined)
                }
              derefCapturedVar(capture, variable)
            case EvaluationStrategy.StaticObject(obj) => getStaticObject(obj)
            case EvaluationStrategy.Field(field) =>
              if field.is(Lazy)
              then
                // a lazy val transformed into a getter method
                callMethod(qualifier, field, Nil)
              else getField(qualifier, field)
            case EvaluationStrategy.FieldAssign(field) =>
              setField(qualifier, field, args.head)
            case EvaluationStrategy.MethodCall(method) =>
              callMethod(qualifier, method, args)
            case EvaluationStrategy.ConstructorCall(ctr, cls) =>
              callConstructor(qualifier, ctr, args)
        case _ => super.transform(tree)

  private def isReflectEval(symbol: Symbol)(using Context): Boolean =
    symbol.name == termName(
      "reflectEval"
    ) && symbol.owner == evalCtx.evaluationClass

  private def derefCapturedVar(tree: Tree, term: TermSymbol)(using
      Context
  ): Tree =
    val typeSymbol = term.info.typeSymbol
    typeSymbol.fullName.toString match
      case s"scala.runtime.${_}Ref" =>
        val elemField = typeSymbol.info.decl(termName("elem")).symbol
        getField(tree, elemField.asTerm)
      case _ => tree

  private def getLocalValue(name: String)(using Context): Tree =
    Apply(
      Select(This(evalCtx.evaluationClass), termName("getLocalValue")),
      List(Literal(Constant(name)))
    )

  private def getOuter(qualifier: Tree, outerCls: ClassSymbol)(using
      Context
  ): Tree =
    Apply(
      Select(This(evalCtx.evaluationClass), termName("getOuter")),
      List(qualifier, Literal(Constant(JavaEncoding.encode(outerCls))))
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
      .map(field => getField(qualifier, field.asTerm))

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

  private def getField(qualifier: Tree, field: TermSymbol)(using
      Context
  ): Tree =
    Apply(
      Select(This(evalCtx.evaluationClass), termName("getField")),
      List(
        qualifier,
        Literal(Constant(JavaEncoding.encode(field.owner))),
        Literal(Constant(field.name.toString))
      )
    )

  private def setField(qualifier: Tree, field: TermSymbol, arg: Tree)(using
      Context
  ): Tree =
    Apply(
      Select(This(evalCtx.evaluationClass), termName("setField")),
      List(
        qualifier,
        Literal(Constant(JavaEncoding.encode(field.owner))),
        Literal(Constant(field.name.toString)),
        arg
      )
    )

  private def callMethod(
      qualifier: Tree,
      method: TermSymbol,
      args: List[Tree]
  )(using Context): Tree =
    val methodType = method.info.asInstanceOf[MethodType]
    val paramTypesNames = methodType.paramInfos.map(JavaEncoding.encode)
    val paramTypesArray = JavaSeqLiteral(
      paramTypesNames.map(t => Literal(Constant(t))),
      TypeTree(ctx.definitions.StringType)
    )

    val capturedArgs =
      methodType.paramNames.dropRight(args.size).map {
        case name @ DerivedName(underlying, _) =>
          capturedValue(method, underlying)
            .getOrElse {
              report.error(s"Unknown captured variable $name in $method")
              ref(defn.Predef_undefined)
            }
        case name =>
          report.error(s"Unknown captured variable $name in $method")
          ref(defn.Predef_undefined)
      }

    val returnTypeName = JavaEncoding.encode(methodType.resType)
    Apply(
      Select(This(evalCtx.evaluationClass), termName("callMethod")),
      List(
        qualifier,
        Literal(Constant(JavaEncoding.encode(method.owner))),
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
      args: List[Tree]
  )(using Context): Tree =
    val methodType = ctr.info.asInstanceOf[MethodType]
    val paramTypesNames = methodType.paramInfos.map(JavaEncoding.encode)
    val clsName = JavaEncoding.encode(methodType.resType)

    val capturedArgs =
      methodType.paramNames.dropRight(args.size).map {
        case outer if outer.toString == "$outer" => qualifier
        case name @ DerivedName(
              underlying,
              _
            ) => // if derived then probably a capture
          capturedValue(ctr.owner, underlying)
            .getOrElse {
              report.error(
                s"Unknown captured variable $name in $ctr of ${ctr.owner}"
              )
              ref(defn.Predef_undefined)
            }
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
    val target = evalCtx.classOwners.indexOf(cls)
    val qualifier = evalCtx.classOwners
      .drop(1)
      .take(target)
      .foldLeft(getLocalValue("$this"))((q, cls) => getOuter(q, cls))
    getClassCapture(qualifier, originalName, cls)

object ResolveReflectEval:
  val name = "resolve-reflect-eval"
