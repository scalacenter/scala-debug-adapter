package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import com.sun.jdi._
import scala.meta.{Type => _, _}
import scala.meta.parsers.*
import scala.meta.trees.*
import MatchingMethods.*
import RuntimeEvaluatorBooleanPatterns.*
import scala.util.Try
import scala.util.Failure
import scala.util.Success

/* Problems
 * The search among all classes impacts the performance (there's a double search when evaluating java code...)
 */
case class RuntimeEvaluator(
    frame: JdiFrame,
    logger: Logger
) {
  private lazy val vm = frame.current().virtualMachine()
  private val thisTree = Validation.fromOption(frame.thisObject.map(ThisTree(_)))

  private def parse(expression: String): Validation[Stat] = {
    expression match {
      case InvalidBegins() | InvalidTokens() =>
        Recoverable("Cannot define types or use pattern matching / implicits at runtime")
      case _: String =>
        expression.parse[Stat] match {
          case err: Parsed.Error => Unrecoverable(err.details)
          case Parsed.Success(tree) => Valid(tree)
          case _: Parsed.Success[?] =>
            Unrecoverable(new Exception("Parsed expression is not a statement"))
        }
    }
  }

  def validate(expression: String): Validation[RuntimeValidationTree] = {
    parse(expression).flatMap { tree =>
      val validated = RuntimeValidation.validate(tree)
      validated match {
        case Unrecoverable(e) => Recoverable(s"Can't validate at runtime, recovering with compiler: $e")
        case _ => validated
      }
    }
  }

  def evaluate(expression: RuntimeValidationTree): Safe[JdiValue] = RuntimeEvaluation.evaluate(expression)

  /* -------------------------------------------------------------------------- */

  private object RuntimeEvaluation {
    def evaluate(stat: RuntimeValidationTree): Safe[JdiValue] =
      stat match {
        case LocalVarTree(localVar) => Safe(frame.variableValue(localVar))
        case primitive: PrimitiveMethodTree => invokePrimitiveMethod(primitive)
        case module: ModuleTree => evaluateModule(module)
        case LiteralTree(value) => Safe(value)
        case ThisTree(obj) => Safe(obj)
        case field: InstanceFieldTree => evaluateField(field)
        case staticField: StaticFieldTree => evaluateStaticField(staticField)
        case instance: NewInstanceTree => instantiate(instance)
        case method: InstanceMethodTree => invokeMethod(method)
        case staticMethod: StaticMethodTree => invokeStaticMethod(staticMethod)
        case _: ClassTree => Safe(throw new Exception("A Class name cannot be evaluated"))
      }

    /* -------------------------------------------------------------------------- */
    /*                              Field evaluation                              */
    /* -------------------------------------------------------------------------- */
    @inline private def evaluateField(tree: InstanceFieldTree): Safe[JdiValue] =
      evaluate(tree.qual).map { value =>
        JdiValue(value.asObject.reference.getValue(tree.field), frame.thread)
      }

    @inline private def evaluateStaticField(tree: StaticFieldTree): Safe[JdiValue] = Safe {
      JdiValue(tree.on.getValue(tree.field), frame.thread)
    }

    /* -------------------------------------------------------------------------- */
    /*                              Method evaluation                             */
    /* -------------------------------------------------------------------------- */
    @inline private def invokeStaticMethod(tree: StaticMethodTree): Safe[JdiValue] =
      for {
        args <- tree.args.map(evaluate).traverse
        argsBoxedIfNeeded <- JdiObject.boxUnboxOnNeed(tree.method.argumentTypes(), args, frame)
        result <- JdiClass(tree.on, frame.thread).invokeStatic(tree.method, argsBoxedIfNeeded)
      } yield result

    @inline private def invokePrimitiveMethod(
        tree: PrimitiveMethodTree
    ): Safe[JdiValue] =
      for {
        lhs <- evaluate(tree.lhs).flatMap(_.unboxIfPrimitive)
        rhs <- evaluate(tree.rhs).flatMap(_.unboxIfPrimitive)
        loader <- frame.classLoader()
        result <- tree.op.evaluate(lhs, rhs, loader)
      } yield result

    @inline private def invokeMethod(tree: InstanceMethodTree): Safe[JdiValue] =
      for {
        qualValue <- evaluate(tree.qual)
        argsValues <- tree.args.map(evaluate).traverse
        argsBoxedIfNeeded <- JdiObject.boxUnboxOnNeed(tree.method.argumentTypes(), argsValues, frame)
        result <- qualValue.asObject.invoke(tree.method, argsBoxedIfNeeded)
      } yield result

    /* -------------------------------------------------------------------------- */
    /*                              Module evaluation                             */
    /* -------------------------------------------------------------------------- */
    @inline private def evaluateModule(tree: ModuleTree): Safe[JdiObject] =
      Safe(JdiObject(tree.`type`.instances(1).get(0), frame.thread))

    /* -------------------------------------------------------------------------- */
    /*                                Instantiation                               */
    /* -------------------------------------------------------------------------- */
    @inline private def instantiate(tree: NewInstanceTree): Safe[JdiObject] =
      for {
        args <- tree.args.map(evaluate).traverse
        boxedUnboxedArgs <- JdiObject.boxUnboxOnNeed(tree.method.argumentTypes(), args, frame)
        instance <- JdiClass(tree.`type`, frame.thread).newInstance(tree.method, boxedUnboxedArgs)
      } yield instance
  }

  /* -------------------------------------------------------------------------- */

  private object RuntimeValidation {
    case class Call(fun: Term, argClause: Term.ArgClause)
    case class PreparedCall(qual: Validation[RuntimeValidationTree], name: String)

    def validate(expression: Stat): Validation[RuntimeValidationTree] = {
      expression match {
        case value: Term.Name => validateName(value)
        case _: Term.This => thisTree.map(tree => ThisTree(tree.value))
        case sup: Term.Super => ???
        case _: Term.Apply | _: Term.ApplyInfix | _: Term.ApplyUnary => validateMethod(extractCall(expression))
        case select: Term.Select => validateSelect(select)
        case newValue: Defn.Val => ???
        case lit: Lit => Valid(LiteralTree(MatchingMethods.fromLitToValue(frame, lit)))
        case instance: Term.New => validateNew(instance)
      }
    }

    /* -------------------------------------------------------------------------- */
    /*                               Name validation                              */
    /* -------------------------------------------------------------------------- */
    @inline private def extractElementIfReference(
        tree: Validation[RuntimeValidationTree]
    )(f: ReferenceType => Validation[RuntimeEvaluationTree]) =
      tree match {
        case ReferenceTree(ref) => f(ref)
        case _: Invalid => staticCallError
        case t => illegalAccess(t, "ReferenceType")
      }

    @inline private def varTreeByName(name: String): Validation[RuntimeEvaluationTree] =
      Validation.fromOption(frame.variableByName(name)).map(LocalVarTree(_))

    @inline private def fieldTreeByName(
        of: Validation[RuntimeValidationTree],
        name: String
    ): Validation[RuntimeEvaluationTree] =
      extractElementIfReference(of) { ref =>
        for {
          field <- Validation(ref.fieldByName(name))
          _ = loadClassOnNeed(field)
        } yield field match {
          case Module(module) => ModuleTree(module, of.toOption)
          case field => toStaticIfNeeded(field, of.get)
        }
      }

    @inline private def zeroArgMethodTreeByName(
        of: Validation[RuntimeValidationTree],
        name: String
    ): Validation[RuntimeEvaluationTree] =
      extractElementIfReference(of) { ref =>
        for {
          method <- methodsByNameAndArgs(ref, name, List.empty, frame)
          _ = loadClassOnNeed(method)
        } yield toStaticIfNeeded(method, List.empty, of.get)
      }

    private def validateClassOrModule(
        name: String,
        of: Option[RuntimeValidationTree]
    ): Validation[TypeTree] = {
      def lookup = {
        val typeName = of.map(_.`type`.name())
        val moduleName = if (name.endsWith("$")) name else name + "$"
        val clsName = name.stripSuffix("$")
        searchAllClassesFor(moduleName, typeName)
          .map(ModuleTree(_, of))
          .orElse {
            searchAllClassesFor(clsName, typeName)
              .map(ClassTree(_))
          }
      }

      of match {
        case Some(_: TypeTree) =>
          lookup.flatMap {
            case cls if cls.`type`.isStatic() => Valid(cls)
            case cls => Recoverable(s"Cannot access non-static class from class type $cls")
          }
        case Some(_) | None => lookup
      }
    }

    private def validateName(
        value: Term.Name
    ): Validation[RuntimeValidationTree] = {
      val name = value.value
      varTreeByName(name)
        .orElse(fieldTreeByName(thisTree, name))
        .orElse(zeroArgMethodTreeByName(thisTree, name))
        .orElse(validateClassOrModule(name, thisTree.toOption))
    }

    /* -------------------------------------------------------------------------- */
    /*                              Apply validation                              */
    /* -------------------------------------------------------------------------- */
    @inline private def extractCall(apply: Stat): Call =
      apply match {
        case apply: Term.Apply => Call(apply.fun, apply.argClause)
        case ColonEndingInfix(apply) => Call(Term.Select(apply.argClause.head, apply.op), List(apply.lhs))
        case apply: Term.ApplyInfix => Call(Term.Select(apply.lhs, apply.op), apply.argClause)
        case apply: Term.ApplyUnary => Call(Term.Select(apply.arg, Term.Name("unary_" + apply.op)), List.empty)
      }

    @inline private def validatePrimitiveMethod(
        name: String,
        lhs: Validation[RuntimeValidationTree],
        rhs: Validation[RuntimeValidationTree]
    ): Validation[RuntimeEvaluationTree] =
      for {
        left <- lhs
        right <- rhs
        op <- Validation.fromOption(RuntimePrimitiveOp(left, right, name))
      } yield PrimitiveMethodTree(left, right, op)

    @inline private def validateApplyCall(
        moduleName: String,
        on: RuntimeValidationTree,
        args: Seq[RuntimeValidationTree]
    ): Validation[MethodTree] =
      for {
        module <- validateClassOrModule(moduleName, Some(on))
        applyCall <- methodsByNameAndArgs(module.`type`, "apply", args.map(_.`type`), frame)
      } yield toStaticIfNeeded(applyCall, args, module)

    private def validateImplicitApplyCall(
        ref: ReferenceType,
        on: RuntimeValidationTree,
        name: String,
        args: Seq[RuntimeValidationTree]
    ): Validation[MethodTree] =
      on match {
        case _: ClassTree => Recoverable("Cannot apply instance method on class")
        case eval: RuntimeEvaluationTree =>
          for {
            method <- methodsByNameAndArgs(ref, name, List.empty, frame)
            methodTree = InstanceMethodTree(method, Seq.empty, eval)
            apply <- validateApplyCall(method.returnTypeName(), methodTree, args)
          } yield apply
      }

    // TODO: should look into upper classes as well
    private def findMethod(
        tree: RuntimeValidationTree,
        name: String,
        args: Seq[RuntimeValidationTree]
    ): Validation[MethodTree] =
      tree.`type` match {
        case ref: ReferenceType =>
          methodsByNameAndArgs(ref, name, args.map(_.`type`), frame)
            .map(toStaticIfNeeded(_, args, tree))
            .orElse(validateApplyCall(name, tree, args))
            .orElse(validateImplicitApplyCall(ref, tree, name, args))
        case t => illegalAccess(t, "ReferenceType")
      }

    private def validateMethod(call: Call): Validation[RuntimeEvaluationTree] = {
      lazy val preparedCall = call.fun match {
        case select: Term.Select => PreparedCall(validate(select.qual), select.name.value)
        case name: Term.Name => PreparedCall(thisTree, name.value)
      }

      // TODO: refactor this (potentially overridden method)
      if (preparedCall.name == "unary_!")
        validatePrimitiveMethod("!", preparedCall.qual, preparedCall.qual)
      else if (call.argClause.size == 1 && RuntimePrimitiveOp.allOperations.contains(preparedCall.name))
        validatePrimitiveMethod(preparedCall.name, preparedCall.qual, validate(call.argClause.head))
      else
        for {
          args <- call.argClause.map(validate(_)).traverse
          qual <- preparedCall.qual
          methodTree <- findMethod(qual, preparedCall.name, args)
          _ = loadClassOnNeed(methodTree.method)
        } yield methodTree
    }

    /* -------------------------------------------------------------------------- */
    /*                              Select validation                             */
    /* -------------------------------------------------------------------------- */
    private def validateSelect(select: Term.Select): Validation[RuntimeValidationTree] = {
      val name = select.name.value
      def getSelectedTerm(of: RuntimeValidationTree) =
        fieldTreeByName(Valid(of), name)
          .orElse(zeroArgMethodTreeByName(Valid(of), name))
          .orElse(validateClassOrModule(name, Some(of)))

      for {
        qual <- validate(select.qual)
        selected <- getSelectedTerm(qual)
      } yield selected
    }

    /* -------------------------------------------------------------------------- */
    /*                               New validation                               */
    /* -------------------------------------------------------------------------- */
    private def validateNew(newValue: Term.New): Validation[RuntimeEvaluationTree] = {
      val name = newValue.init.tpe.toString
      val argClauses = newValue.init.argClauses

      for {
        args <- argClauses.flatMap(_.map(validate(_))).traverse
        cls <- searchAllClassesFor(name, None)
        method <- methodsByNameAndArgs(cls, "<init>", args.map(_.`type`), frame, encode = false)
      } yield NewInstanceTree(method, args)
    }

    /* -------------------------------------------------------------------------- */
    /*                                   Helpers                                  */
    /* -------------------------------------------------------------------------- */
    @inline private def staticCallError = Recoverable("Calling from static context, 'this' unavailable")
    @inline private def illegalAccess(x: Any, typeName: String) = Unrecoverable {
      new ClassCastException(s"Cannot cast $x to $typeName")
    }

    @inline private def toStaticIfNeeded(field: Field, on: RuntimeValidationTree): FieldTree = on match {
      case cls: ClassTree => StaticFieldTree(field, cls.`type`)
      case eval: RuntimeEvaluationTree => InstanceFieldTree(field, eval)
    }

    @inline private def toStaticIfNeeded(
        method: Method,
        args: Seq[RuntimeValidationTree],
        on: RuntimeValidationTree
    ): MethodTree = on match {
      case cls: ClassTree => StaticMethodTree(method, args, cls.`type`)
      case eval: RuntimeEvaluationTree => InstanceMethodTree(method, args, eval)
    }

    @inline private def loadClass(name: String): Safe[JdiClass] = frame.classLoader().flatMap(_.loadClass(name))
    // TODO: better name
    private def checkClass(tpe: => Type)(name: String) = Try(tpe) match {
      case Failure(_: ClassNotLoadedException) => loadClass(name).getResult.map(_.cls)
      case Success(value: ClassType) if !value.isPrepared => loadClass(name).getResult.map(_.cls)
      case result => result
    }

    private def loadClassOnNeed(tc: TypeComponent): Try[Type] = tc match {
      case f: Field => checkClass(f.`type`())(f.typeName())
      case m: Method => checkClass(m.returnType())(m.returnTypeName())
    }

    private def searchAllClassesFor(name: String, in: Option[String]): Validation[ClassType] = {
      def fullName = in match {
        case Some(value) if value == name => name // name duplication when implicit apply call
        case Some(value) if value.endsWith("$") => value + name
        case Some(value) => value + "$" + name
        case None => name
      }

      def nameEndMatch(cls: String) =
        (name.endsWith("$"), cls.endsWith("$")) match {
          case (true, true) | (false, false) =>
            cls.split('.').last.split('$').last == name.stripSuffix("$")
          case (true, false) | (false, true) => false
        }

      def candidates =
        vm.allClasses()
          .asScalaSeq
          .filter { cls => cls.name() == name || nameEndMatch(cls.name()) }

      def finalCandidates =
        candidates.size match {
          case 0 => loadClass(fullName).map(_.cls).getResult.toSeq
          case 1 => candidates
          case _ => candidates.filter(_.name() == fullName)
        }

      finalCandidates
        .toValidation(s"Cannot find module/class $fullName")
        .map { cls => checkClass(cls)(cls.name()).get.asInstanceOf[ClassType] }
    }
  }
}

/* -------------------------------------------------------------------------- */

private object MatchingMethods {
  def fromLitToValue(frame: JdiFrame, literal: Lit): JdiValue = {
    val vm = frame.current().virtualMachine()
    val value = literal match {
      case Lit.Boolean(value) => vm.mirrorOf(value)
      case Lit.Byte(value) => vm.mirrorOf(value)
      case Lit.Char(value) => vm.mirrorOf(value)
      case Lit.Double(value) => vm.mirrorOf(value.toDouble)
      case Lit.Float(value) => vm.mirrorOf(value.toFloat)
      case Lit.Int(value) => vm.mirrorOf(value)
      case Lit.Long(value) => vm.mirrorOf(value)
      case Lit.Short(value) => vm.mirrorOf(value)
      case Lit.String(value) => vm.mirrorOf(value)
      case _ => throw new Exception(s"Unsupported literal: $literal")
    }

    JdiValue(value, frame.thread)
  }

  /* -------------------------------------------------------------------------- */
  /*                                Method lookup                               */
  /* -------------------------------------------------------------------------- */
  @inline private def argsMatch(method: Method, args: Seq[Type], frame: JdiFrame): Boolean =
    method.argumentTypeNames().size() == args.size && sameTypes(method, args, frame)

  def methodsByNameAndArgs(
      ref: ReferenceType,
      funName: String,
      args: Seq[Type],
      frame: JdiFrame,
      encode: Boolean = true
  ): Validation[Method] =
    ref
      .methodsByName { if (encode) NameTransformer.encode(funName) else funName }
      .asScalaSeq
      .filter(method => !method.isPrivate && argsMatch(method, args, frame))
      .toSeq
      .toValidation(s"Cannot find methods $funName with args types $args")

  /* -------------------------------------------------------------------------- */
  /*                                Type checker                                */
  /* -------------------------------------------------------------------------- */
  def sameType(got: Type, expected: Type, frame: JdiFrame): Boolean = {
    def referenceTypesMatch(got: ReferenceType, expected: ReferenceType) = {
      val assignableFrom = expected.classObject().referenceType().methodsByName("isAssignableFrom").get(0)
      val params = Seq(got.classObject()).asJavaList
      expected.classObject
        .invokeMethod(frame.thread, assignableFrom, params, ObjectReference.INVOKE_SINGLE_THREADED)
        .asInstanceOf[BooleanValue]
        .value()
    }

    (got, expected) match {
      case (g: ArrayType, at: ArrayType) => sameType(g.componentType, at.componentType, frame)
      case (g: PrimitiveType, pt: PrimitiveType) => got.equals(pt)
      case (g: ReferenceType, ref: ReferenceType) => referenceTypesMatch(g, ref)
      case (_: VoidType, _: VoidType) => true

      case (g: ClassType, pt: PrimitiveType) =>
        sameType(g, frame.getPrimitiveBoxedClass(pt), frame)
      case (g: PrimitiveType, ct: ReferenceType) =>
        sameType(frame.getPrimitiveBoxedClass(g), ct, frame)

      case _ => false
    }
  }

  @inline def sameTypes(method: Method, args: Seq[Type], frame: JdiFrame): Boolean =
    method.argumentTypes().asScalaSeq.zip(args).forall { case (expected, got) =>
      sameType(got, expected, frame)
    }
}
