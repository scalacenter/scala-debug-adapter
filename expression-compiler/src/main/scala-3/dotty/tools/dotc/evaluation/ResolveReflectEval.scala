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
import dotty.tools.dotc.core.Phases
import dotty.tools.dotc.core.TypeErasure.ErasedValueType
import dotty.tools.dotc.transform.ValueClasses

class ResolveReflectEval(using evalCtx: EvaluationContext) extends MiniPhase:
  override def phaseName: String = ResolveReflectEval.name

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    if tree.symbol == evalCtx.evaluationClass
    then ExpressionTransformer.transform(tree)
    else tree

  object ExpressionTransformer extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case tree: DefDef if tree.symbol == evalCtx.evaluateMethod =>
          // unbox the result of the `evaluate` method if it is a value class
          val rhs = unboxIfValueClass(
            evalCtx.expressionSymbol,
            transform(tree.rhs)
          )
          cpy.DefDef(tree)(rhs = rhs)

        case tree: Apply if isReflectEval(tree.fun.symbol) =>
          val qualifier :: _ :: argsTree :: Nil = tree.args.map(transform)
          val args = argsTree.asInstanceOf[JavaSeqLiteral].elems
          tree.attachment(EvaluationStrategy) match
            case EvaluationStrategy.This(cls) =>
              if cls.isValueClass then
                // if cls is a value class then the local $this is the erased value,
                // but we expect an instance of the value class instead
                boxValueClass(cls, getLocalValue("$this"))
              else getLocalValue("$this")
            case EvaluationStrategy.Outer(outerCls) =>
              getOuter(qualifier, outerCls)
            case EvaluationStrategy.LocalValue(variable) =>
              val variableName = JavaEncoding.encode(variable.name)
              val localValue = derefCapturedVar(
                getLocalValue(variableName),
                variable
              )
              boxIfValueClass(variable, localValue)
            case EvaluationStrategy.LocalValueAssign(variable) =>
              val value = unboxIfValueClass(variable, args.head)
              val typeSymbol = variable.info.typeSymbol.asType
              val variableName = JavaEncoding.encode(variable.name)
              JavaEncoding.encode(typeSymbol) match
                case s"scala.runtime.${_}Ref" =>
                  val elemField = typeSymbol.info.decl(termName("elem")).symbol
                  setField(
                    getLocalValue(variableName),
                    elemField.asTerm,
                    value
                  )
                case _ => setLocalValue(variableName, value)
            case EvaluationStrategy.ClassCapture(variable, cls) =>
              val capture = getClassCapture(qualifier, variable.name, cls)
                .getOrElse {
                  report.error(
                    s"No capture found for $variable in $cls",
                    tree.srcPos
                  )
                  ref(defn.Predef_undefined)
                }
              val capturedValue = derefCapturedVar(capture, variable)
              boxIfValueClass(variable, capturedValue)
            case EvaluationStrategy.ClassCaptureAssign(variable, cls) =>
              val capture = getClassCapture(qualifier, variable.name, cls)
                .getOrElse {
                  report.error(
                    s"No capture found for $variable in $cls",
                    tree.srcPos
                  )
                  ref(defn.Predef_undefined)
                }
              val value = unboxIfValueClass(variable, args.head)
              val typeSymbol = variable.info.typeSymbol
              val elemField = typeSymbol.info.decl(termName("elem")).symbol
              setField(capture, elemField.asTerm, value)
            case EvaluationStrategy.MethodCapture(variable, method) =>
              val capture = getMethodCapture(method, variable.name)
                .getOrElse {
                  report.error(
                    s"No capture found for $variable in $method",
                    tree.srcPos
                  )
                  ref(defn.Predef_undefined)
                }
              val capturedValue = derefCapturedVar(capture, variable)
              boxIfValueClass(variable, capturedValue)
            case EvaluationStrategy.MethodCaptureAssign(variable, method) =>
              val capture = getMethodCapture(method, variable.name)
                .getOrElse {
                  report.error(
                    s"No capture found for $variable in $method",
                    tree.srcPos
                  )
                  ref(defn.Predef_undefined)
                }
              val value = unboxIfValueClass(variable, args.head)
              val typeSymbol = variable.info.typeSymbol
              val elemField = typeSymbol.info.decl(termName("elem")).symbol
              setField(capture, elemField.asTerm, value)
            case EvaluationStrategy.StaticObject(obj) => getStaticObject(obj)
            case EvaluationStrategy.Field(field) =>
              // if the field is lazy, if it is private in a value class or a trait
              // then we must call the getter method
              val fieldValue =
                if field.is(Lazy) ||
                  field.owner.isValueClass ||
                  field.owner.is(Trait)
                then callMethod(tree)(qualifier, field.getter.asTerm, Nil)
                else getField(qualifier, field)
              boxIfValueClass(field, fieldValue)
            case EvaluationStrategy.FieldAssign(field) =>
              val arg = unboxIfValueClass(field, args.head)
              if field.owner.is(Trait) then
                callMethod(tree)(qualifier, field.setter.asTerm, List(arg))
              else setField(qualifier, field, arg)
            case EvaluationStrategy.MethodCall(method) =>
              callMethod(tree)(qualifier, method, args)
            case EvaluationStrategy.ConstructorCall(ctr, cls) =>
              callConstructor(tree)(qualifier, ctr, args)
        case _ => super.transform(tree)

  private def isReflectEval(symbol: Symbol)(using Context): Boolean =
    symbol.name == termName(
      "reflectEval"
    ) && symbol.owner == evalCtx.evaluationClass

  private def derefCapturedVar(tree: Tree, term: TermSymbol)(using
      Context
  ): Tree =
    val typeSymbol = term.info.typeSymbol.asType
    JavaEncoding.encode(typeSymbol) match
      case s"scala.runtime.${_}Ref" =>
        val elemField = typeSymbol.info.decl(termName("elem")).symbol
        getField(tree, elemField.asTerm)
      case _ => tree

  private def boxIfValueClass(term: TermSymbol, tree: Tree)(using
      Context
  ): Tree =
    atPhase(Phases.elimErasedValueTypePhase)(term.info) match
      case tpe: ErasedValueType =>
        boxValueClass(tpe.tycon.typeSymbol.asClass, tree)
      case tpe => tree

  private def boxValueClass(valueClass: ClassSymbol, tree: Tree)(using
      Context
  ): Tree =
    // qualifier is null: a value class cannot be nested into a class
    val ctor = valueClass.primaryConstructor.asTerm
    callConstructor(tree)(nullLiteral, ctor, List(tree))

  private def unboxIfValueClass(term: TermSymbol, tree: Tree)(using
      Context
  ): Tree =
    atPhase(Phases.elimErasedValueTypePhase)(term.info) match
      case tpe: ErasedValueType => unboxValueClass(tree, tpe)
      case tpe => tree

  private def unboxValueClass(tree: Tree, tpe: ErasedValueType)(using
      Context
  ): Tree =
    val cls = tpe.tycon.typeSymbol.asClass
    val unboxMethod = ValueClasses.valueClassUnbox(cls).asTerm
    callMethod(tree)(tree, unboxMethod, Nil)

  private def getLocalValue(name: String)(using Context): Tree =
    Apply(
      Select(This(evalCtx.evaluationClass), termName("getLocalValue")),
      List(Literal(Constant(name)))
    )

  private def setLocalValue(name: String, value: Tree)(using Context): Tree =
    Apply(
      Select(This(evalCtx.evaluationClass), termName("setLocalValue")),
      List(Literal(Constant(name)), value)
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
      .map { param =>
        val paramName = JavaEncoding.encode(param)
        getLocalValue(paramName)
      }

  private def getStaticObject(obj: ClassSymbol)(using Context): Tree =
    val className = JavaEncoding.encode(obj)
    Apply(
      Select(This(evalCtx.evaluationClass), termName("getStaticObject")),
      List(Literal(Constant(className)))
    )

  private def getField(qualifier: Tree, field: TermSymbol)(using
      Context
  ): Tree =
    val fieldName = JavaEncoding.encode(field.name)
    Apply(
      Select(This(evalCtx.evaluationClass), termName("getField")),
      List(
        qualifier,
        Literal(Constant(JavaEncoding.encode(field.owner.asType))),
        Literal(Constant(fieldName))
      )
    )

  private def setField(qualifier: Tree, field: TermSymbol, value: Tree)(using
      Context
  ): Tree =
    val fieldName = JavaEncoding.encode(field.name)
    Apply(
      Select(This(evalCtx.evaluationClass), termName("setField")),
      List(
        qualifier,
        Literal(Constant(JavaEncoding.encode(field.owner.asType))),
        Literal(Constant(fieldName)),
        value
      )
    )

  private def callMethod(tree: Tree)(
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
              report.error(
                s"Unknown captured variable $name in $method",
                tree.srcPos
              )
              ref(defn.Predef_undefined)
            }
        case name =>
          report
            .error(s"Unknown captured variable $name in $method", tree.srcPos)
          ref(defn.Predef_undefined)
      }

    val erasedMethodInfo = atPhase(Phases.elimErasedValueTypePhase)(method.info)
      .asInstanceOf[MethodType]
    val unboxedArgs =
      erasedMethodInfo.paramInfos.takeRight(args.size).zip(args).map {
        case (tpe: ErasedValueType, arg) => unboxValueClass(arg, tpe)
        case (_, arg) => arg
      }

    val returnTypeName = JavaEncoding.encode(methodType.resType)
    val methodName = JavaEncoding.encode(method.name)
    val result = Apply(
      Select(This(evalCtx.evaluationClass), termName("callMethod")),
      List(
        qualifier,
        Literal(Constant(JavaEncoding.encode(method.owner.asType))),
        Literal(Constant(methodName)),
        paramTypesArray,
        Literal(Constant(returnTypeName)),
        JavaSeqLiteral(
          capturedArgs ++ unboxedArgs,
          TypeTree(ctx.definitions.ObjectType)
        )
      )
    )
    erasedMethodInfo.resType match
      case tpe: ErasedValueType =>
        boxValueClass(tpe.tycon.typeSymbol.asClass, result)
      case _ => result

  private def callConstructor(tree: Tree)(
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
        case name @ DerivedName(underlying, _) =>
          // if derived then probably a capture
          capturedValue(ctr.owner, underlying)
            .getOrElse {
              report.error(
                s"Unknown captured variable $name in $ctr of ${ctr.owner}",
                tree.srcPos
              )
              ref(defn.Predef_undefined)
            }
        case name =>
          val paramName = JavaEncoding.encode(name)
          getLocalValue(paramName)
      }

    val erasedCtrInfo = atPhase(Phases.elimErasedValueTypePhase)(ctr.info)
      .asInstanceOf[MethodType]
    val unboxedArgs =
      erasedCtrInfo.paramInfos.takeRight(args.size).zip(args).map {
        case (tpe: ErasedValueType, arg) => unboxValueClass(arg, tpe)
        case (_, arg) => arg
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
          capturedArgs ++ unboxedArgs,
          TypeTree(ctx.definitions.ObjectType)
        )
      )
    )

  private def capturedValue(sym: Symbol, originalName: TermName)(using
      Context
  ): Option[Tree] =
    val encodedName = JavaEncoding.encode(originalName)
    if evalCtx.classOwners.contains(sym)
    then capturedByClass(sym.asClass, originalName)
    else
    // if the captured value is not a local variables
    // then it must have been captured by the outer method
    if evalCtx.localVariables.contains(encodedName)
    then Some(getLocalValue(encodedName))
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
