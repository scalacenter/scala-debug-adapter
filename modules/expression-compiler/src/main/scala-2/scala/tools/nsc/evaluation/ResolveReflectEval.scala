package scala.tools.nsc.evaluation

import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform
import scala.util.matching.Regex

class ResolveReflectEval(override val global: ExpressionGlobal)
    extends Transform
    with TypingTransformers
    with JavaEncoding {
  import global._

  override val phaseName: String = "resolve-reflect-eval"
  override val runsAfter: List[String] = List("delambdafy")
  override val runsBefore: List[String] = List("jvm")
  override val runsRightAfter: Option[String] = None

  override protected def newTransformer(unit: CompilationUnit): Transformer = new TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      case tree: DefDef if tree.symbol == evaluate =>
        // unbox the result of the `evaluate` method if it is a value class
        val reflectEvalApply = global.gen
          .mkMethodCall(reflectEval, List(mkNullLiteral, mkNullLiteral, mkNullLiteral))
          .asInstanceOf[Apply]
          .copyAttrs(tree.rhs)
        val gen = new Gen(reflectEvalApply)
        val rhs = gen.unboxIfValueClass(expressionVal, transform(tree.rhs))
        tree.copy(rhs = rhs).copyAttrs(tree)

      case apply: Apply if apply.fun.symbol == reflectEval =>
        val qualifier :: _ :: argsTree :: Nil = apply.args.map(transform): @unchecked
        val args = argsTree.asInstanceOf[ArrayValue].elems
        val gen = new Gen(apply)
        tree.attachments.get[EvaluationStrategy].get match {
          case _: EvaluationStrategy.This => gen.mkGetThisObject
          case _: EvaluationStrategy.LocalOuter => gen.mkGetLocalValue("$outer")
          case EvaluationStrategy.Outer(outerCls) => gen.mkGetOuter(qualifier, outerCls)
          case EvaluationStrategy.LocalValue(variable, isByName) =>
            val rawLocalValue = gen.mkGetLocalValue(variable.name.encoded)
            val localValue = if (isByName) gen.mkByNameApply(rawLocalValue) else rawLocalValue
            val derefLocalValue = gen.derefCapturedVar(localValue, variable)
            gen.boxIfValueClass(variable, derefLocalValue)
          case EvaluationStrategy.LocalValueAssign(variable) =>
            val value = gen.unboxIfValueClass(variable, args.head)
            val typeSymbol = variable.info.typeSymbol.asType
            val variableName = variable.name.encoded
            val Ref = s"scala.runtime.(.*)Ref".r
            encode(typeSymbol) match {
              case Ref(_) =>
                val elemField = typeSymbol.info.decl(TermName("elem"))
                gen.mkSetField(gen.mkGetLocalValue(variableName), elemField.asTerm, value)
              case _ => gen.mkSetLocalValue(variableName, value)
            }
          case EvaluationStrategy.ClassCapture(variable, cls, isByName) =>
            val rawCapture = gen
              .mkGetClassCapture(qualifier, variable.name, cls)
              .getOrElse {
                reporter.error(tree.pos, s"No capture found for $variable in $cls")
                mkUndefined
              }
            val capture = if (isByName) gen.mkByNameApply(rawCapture) else rawCapture
            val capturedValue = gen.derefCapturedVar(capture, variable)
            gen.boxIfValueClass(variable, capturedValue)
          case EvaluationStrategy.ClassCaptureAssign(variable, cls) =>
            val capture = gen
              .mkGetClassCapture(qualifier, variable.name, cls)
              .getOrElse {
                reporter.error(tree.pos, s"No capture found for $variable in $cls")
                mkUndefined
              }
            val value = gen.unboxIfValueClass(variable, args.head)
            val typeSymbol = variable.info.typeSymbol
            val elemField = typeSymbol.info.decl(TermName("elem"))
            gen.mkSetField(capture, elemField.asTerm, value)
          case EvaluationStrategy.MethodCapture(variable, method, isByName) =>
            val rawCapture = gen
              .mkGetMethodCapture(method, variable.name)
              .getOrElse {
                reporter.error(tree.pos, s"No capture found for $variable in $method")
                mkUndefined
              }
            val capture = if (isByName) gen.mkByNameApply(rawCapture) else rawCapture
            val capturedValue = gen.derefCapturedVar(capture, variable)
            gen.boxIfValueClass(variable, capturedValue)
          case EvaluationStrategy.MethodCaptureAssign(variable, method) =>
            val capture = gen
              .mkGetMethodCapture(method, variable.name)
              .getOrElse {
                reporter.error(tree.pos, s"No capture found for $variable in $method")
                mkUndefined
              }
            val value = gen.unboxIfValueClass(variable, args.head)
            val typeSymbol = variable.info.typeSymbol
            val elemField = typeSymbol.info.decl(TermName("elem"))
            gen.mkSetField(capture, elemField.asTerm, value)
          case EvaluationStrategy.StaticObject(obj) => gen.mkGetStaticObject(obj)
          case EvaluationStrategy.Field(field, isByName) =>
            // if the field is lazy, if it is private in a value class or a trait
            // then we must call the getter method
            val fieldValue =
              if (field.isLazy || field.owner.isDerivedValueClass || field.owner.isTrait)
                gen.mkCallMethod(qualifier, field.getterIn(field.owner).asTerm, Nil)
              else {
                val rawValue = gen.mkGetField(qualifier, field)
                if (isByName) gen.mkByNameApply(rawValue) else rawValue
              }
            gen.boxIfValueClass(field, fieldValue)
          case EvaluationStrategy.FieldAssign(field) =>
            val arg = gen.unboxIfValueClass(field, args.head)
            if (field.owner.isTrait) gen.mkCallMethod(qualifier, field.setterIn(field.owner).asTerm, List(arg))
            else gen.mkSetField(qualifier, field, arg)
          case EvaluationStrategy.MethodCall(method) => gen.mkCallMethod(qualifier, method, args)
          case EvaluationStrategy.ConstructorCall(ctr, cls) => gen.mkCallConstructor(qualifier, ctr, args)
        }
      case _ => super.transform(tree)
    }
  }

  private class Gen(reflectEval: Apply) {

    def derefCapturedVar(tree: Tree, term: TermSymbol): Tree = {
      val typeSymbol = term.info.typeSymbol.asType
      val Ref = s"scala.runtime.(.*)Ref".r
      encode(typeSymbol) match {
        case Ref(_) =>
          val elemField = typeSymbol.info.decl(TermName("elem"))
          mkGetField(tree, elemField.asTerm)
        case _ => tree
      }
    }

    def boxIfValueClass(term: TermSymbol, tree: Tree): Tree =
      getErasedValueType(exitingErasure(term.info)) match {
        case Some(erasedValueType) =>
          boxValueClass(erasedValueType.valueClazz.asClass, tree)
        case None => tree
      }

    def boxValueClass(valueClass: ClassSymbol, tree: Tree): Tree = {
      val ctor = valueClass.primaryConstructor.asTerm
      mkCallConstructor(mkNullLiteral, ctor, List(tree))
    }

    def unboxIfValueClass(term: TermSymbol, tree: Tree): Tree =
      getErasedValueType(exitingErasure(term.info)) match {
        case Some(erasedValueType) => unboxValueClass(tree, erasedValueType)
        case None => tree
      }

    private def getErasedValueType(tpe: Type): Option[ErasedValueType] = tpe match {
      case tpe: ErasedValueType => Some(tpe)
      case tpe: MethodType => getErasedValueType(tpe.resultType)
      case tpe: PolyType => getErasedValueType(tpe.resultType)
      case tpe => None
    }

    private def unboxValueClass(tree: Tree, tpe: ErasedValueType): Tree = {
      val unboxMethod = tpe.valueClazz.derivedValueClassUnbox.asTerm
      mkCallMethod(tree, unboxMethod, Nil)
    }

    def mkGetThisObject: Tree = mkExprMethodCall(getThisObject, List.empty)

    def mkGetLocalValue(name: String): Tree = mkExprMethodCall(getLocalValue, List(mkStringLiteral(name)))

    def mkSetLocalValue(name: String, value: Tree): Tree =
      mkExprMethodCall(setLocalValue, List(mkStringLiteral(name), value))

    def mkGetOuter(qualifier: Tree, outerCls: ClassSymbol): Tree =
      mkExprMethodCall(getOuter, List(qualifier, mkStringLiteral(encode(outerCls))))

    def mkGetClassCapture(qualifier: Tree, originalName: Name, cls: ClassSymbol): Option[Tree] =
      cls.info.decls.iterator
        .filter(term => term.isField)
        .find { field =>
          val DerivedName = s"${Regex.quote(originalName.toString)}\\$$\\d+".r
          val QualifiedName = s".*\\$$${Regex.quote(originalName.toString)}\\$$\\d+".r
          field.name.toString match {
            case DerivedName() => true
            case QualifiedName() => true
            case _ => false
          }
        }
        .map(field => mkGetField(qualifier, field.asTerm))

    def mkGetMethodCapture(method: TermSymbol, originalName: TermName): Option[Tree] = {
      val DerivedName = s"${Regex.quote(originalName.toString)}\\$$\\d+".r
      method.info
        .asInstanceOf[MethodType]
        .params
        .map(_.name)
        .collectFirst { case name @ DerivedName() => mkGetLocalValue(name.encoded) }
    }

    def mkGetStaticObject(obj: ClassSymbol): Tree =
      mkExprMethodCall(getStaticObject, List(mkStringLiteral(encode(obj))))

    def mkGetField(qualifier: Tree, field: TermSymbol): Tree = {
      val args = List(qualifier, mkStringLiteral(encode(field.owner.asType)), mkStringLiteral(field.name.encoded))
      mkExprMethodCall(getField, args)
    }

    def mkSetField(qualifier: Tree, field: TermSymbol, value: Tree): Tree = {
      val args =
        List(qualifier, mkStringLiteral(encode(field.owner.asType)), mkStringLiteral(field.name.encoded), value)
      mkExprMethodCall(setField, args)
    }

    def mkByNameApply(tree: Tree): Tree = {
      val function0Class = definitions.FunctionClass(0)
      val function0Apply = definitions.getMemberMethod(function0Class, TermName("apply"))
      val function = gen.mkAttributedCast(tree, appliedType(definitions.FunctionClass(0), definitions.ObjectTpe))
      gen.mkMethodCall(function, function0Apply, List.empty, List.empty).setType(definitions.ObjectTpe)
    }

    def mkCallMethod(qualifier: Tree, method: TermSymbol, args: List[Tree]): Tree = {
      def unknownCapture(name: Name): Tree = {
        reporter.error(reflectEval.pos, s"Unknown captured variable $name in $method")
        mkUndefined
      }

      val methodType = method.info.asInstanceOf[MethodType]
      val capturedArgs = methodType.params.map(_.name).drop(args.size).map {
        case name @ DerivedName(underlying) => capturedValue(method, underlying).getOrElse(unknownCapture(name))
        case name => unknownCapture(name)
      }

      val unboxedArgs = exitingErasure(method.info.paramTypes).take(args.size).zip(args).map {
        case (tpe: ErasedValueType, arg) => unboxValueClass(arg, tpe)
        case (_, arg) => arg
      }

      val returnTypeName = encode(methodType.resultType)
      val allArgs = List(
        qualifier,
        mkStringLiteral(encode(method.owner.asType)),
        mkStringLiteral(method.name.encoded),
        mkStringLiteralArray(methodType.paramTypes.map(encode)),
        mkStringLiteral(returnTypeName),
        mkObjectArray(unboxedArgs ++ capturedArgs)
      )
      val result = mkExprMethodCall(callMethod, allArgs)
      exitingErasure(method.info.resultType) match {
        case tpe: ErasedValueType => boxValueClass(tpe.valueClazz.asClass, result)
        case _ => result
      }
    }

    def mkCallConstructor(qualifier: Tree, ctr: TermSymbol, args: List[Tree]): Tree = {
      val methodType = ctr.info.asInstanceOf[MethodType]
      val capturedArgs =
        methodType.params.map(_.name).drop(args.size).map {
          case outer if outer.toString == "arg$outer" => qualifier
          case name @ DerivedName(underlying) =>
            // if derived then probably a capture
            capturedValue(ctr.owner, underlying)
              .getOrElse {
                reporter.error(reflectEval.pos, s"Unknown captured variable $name in $ctr of ${ctr.owner}")
                mkUndefined
              }
          case name => mkGetLocalValue(name.encoded)
        }

      val unboxedArgs = exitingErasure(ctr.info.paramTypes).take(args.size).zip(args).map {
        case (tpe: ErasedValueType, arg) => unboxValueClass(arg, tpe)
        case (_, arg) => arg
      }
      val allArgs = List(
        mkStringLiteral(encode(methodType.resultType)),
        mkStringLiteralArray(methodType.paramTypes.map(encode)),
        mkObjectArray(unboxedArgs ++ capturedArgs)
      )
      mkExprMethodCall(callConstructor, allArgs)
    }

    private val expressionThis = reflectEval.fun.asInstanceOf[Select].qualifier
    private def mkExprMethodCall(method: Symbol, args: List[Tree]): Tree =
      gen.mkMethodCall(expressionThis, method, List.empty, args).setType(method.info.resultType)

    private def capturedValue(sym: Symbol, originalName: TermName): Option[Tree] = {
      val encodedName = originalName.encoded
      if (classOwners.contains(sym)) capturedByClass(sym.asClass, originalName)
      else if (localVariables.contains(encodedName)) Some(mkGetLocalValue(encodedName))
      else
        // if the captured value is not a local variables
        // then it must have been captured by the outer method
        capturingMethod.flatMap(mkGetMethodCapture(_, originalName))
    }

    private def capturedByClass(cls: ClassSymbol, originalName: TermName): Option[Tree] = {
      val target = classOwners.indexOf(cls)
      val qualifier = classOwners
        .drop(1)
        .take(target)
        .foldLeft(mkGetThisObject)((q, cls) => mkGetOuter(q, cls))
      mkGetClassCapture(qualifier, originalName, cls)
    }

    private object DerivedName {
      def unapply(name: TermName): Option[TermName] =
        "(.*)\\$\\d+".r
          .unapplySeq(name.toString)
          .collect { case underlying :: Nil => TermName(underlying) }
    }
  }
}
