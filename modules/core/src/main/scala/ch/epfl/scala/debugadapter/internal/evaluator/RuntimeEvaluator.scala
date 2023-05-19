package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import com.sun.jdi._
import scala.meta.{Type => _, _}
import scala.meta.parsers.*
import scala.meta.trees.*
import MatchingMethods.*
import Helpers.*
import RuntimeEvaluatorExtractors.*
import scala.util.Try
import scala.util.Failure
import scala.util.Success

private case class Call(fun: Term, argClause: Term.ArgClause)
private case class PreparedCall(qual: Validation[RuntimeTree], name: String)

case class RuntimeEvaluator(
    frame: JdiFrame,
    logger: Logger
) {
  private val thisTree =
    Validation.fromOption {
      frame.thisObject
        .map { ths => ThisTree(ths.reference.referenceType().asInstanceOf[ClassType]) }
    }

  private def parse(expression: String): Validation[Stat] =
    expression.parse[Stat] match {
      case err: Parsed.Error => Unrecoverable(err.details)
      case Parsed.Success(tree) => Valid(tree)
      case _: Parsed.Success[?] =>
        Unrecoverable(new Exception("Parsed expression is not a statement"))
    }

  def validate(expression: String): Validation[RuntimeEvaluationTree] =
    parse(expression).flatMap {
      RuntimeValidation.validate(_) match {
        case Unrecoverable(e) => Recoverable(s"Can't validate at runtime, recovering with compiler: $e")
        case validated => validated
      }
    }

  def evaluate(expression: RuntimeEvaluationTree): Safe[JdiValue] =
    RuntimeEvaluation.evaluate(expression).map(_.derefIfRef)

  /* -------------------------------------------------------------------------- */

  private object RuntimeEvaluation {
    def evaluate(stat: RuntimeEvaluationTree): Safe[JdiValue] =
      stat match {
        case LocalVarTree(varName, _) => Safe.successful(frame.variableByName(varName).map(frame.variableValue).get)
        case primitive: PrimitiveBinaryOpTree => invokePrimitive(primitive)
        case primitive: PrimitiveUnaryOpTree => invokePrimitive(primitive)
        case module: ModuleTree => evaluateModule(module, module.of.map(evaluate))
        case literal: LiteralTree => evaluateLiteral(literal)
        case ThisTree(obj) => Safe(JdiValue(obj.instances(1).get(0), frame.thread))
        case field: InstanceFieldTree => evaluateField(field)
        case staticField: StaticFieldTree => evaluateStaticField(staticField)
        case instance: NewInstanceTree => instantiate(instance)
        case method: InstanceMethodTree => invoke(method)
        case staticMethod: StaticMethodTree => invokeStatic(staticMethod)
        case outer: OuterTree => evaluateOuter(outer)
      }

    /* -------------------------------------------------------------------------- */
    /*                             Literal evaluation                             */
    /* -------------------------------------------------------------------------- */
    private def evaluateLiteral(tree: LiteralTree): Safe[JdiValue] =
      for {
        loader <- frame.classLoader()
        value <- tree.value
        result <- loader.mirrorOfLiteral(value)
      } yield result

    /* -------------------------------------------------------------------------- */
    /*                              Outer evaluation                              */
    /* -------------------------------------------------------------------------- */
    private def evaluateOuter(tree: OuterTree): Safe[JdiValue] =
      evaluate(tree.inner).flatMap { innerValue =>
        tree match {
          case OuterModuleTree(_, module) =>
            evaluateModule(module, Some(Safe(innerValue)))
          case _: OuterClassTree =>
            Safe(innerValue.asObject.getField("$outer"))
        }

      }

    /* -------------------------------------------------------------------------- */
    /*                              Field evaluation                              */
    /* -------------------------------------------------------------------------- */
    private def evaluateField(tree: InstanceFieldTree): Safe[JdiValue] =
      evaluate(tree.qual).map { value =>
        JdiValue(value.asObject.reference.getValue(tree.field), frame.thread)
      }

    private def evaluateStaticField(tree: StaticFieldTree): Safe[JdiValue] =
      Safe { JdiValue(tree.on.getValue(tree.field), frame.thread) }

    /* -------------------------------------------------------------------------- */
    /*                              Method evaluation                             */
    /* -------------------------------------------------------------------------- */
    private def invokeStatic(tree: StaticMethodTree): Safe[JdiValue] =
      for {
        args <- tree.args.map(evaluate).traverse
        loader <- frame.classLoader()
        argsBoxedIfNeeded <- loader.boxUnboxOnNeed(tree.method.argumentTypes(), args)
        result <- JdiClass(tree.on, frame.thread).invokeStatic(tree.method, argsBoxedIfNeeded)
      } yield result

    private def invokePrimitive(tree: PrimitiveBinaryOpTree): Safe[JdiValue] =
      for {
        lhs <- evaluate(tree.lhs).flatMap(_.unboxIfPrimitive)
        rhs <- evaluate(tree.rhs).flatMap(_.unboxIfPrimitive)
        loader <- frame.classLoader()
        result <- tree.op.evaluate(lhs, rhs, loader)
      } yield result

    private def invokePrimitive(tree: PrimitiveUnaryOpTree): Safe[JdiValue] =
      for {
        rhs <- evaluate(tree.rhs).flatMap(_.unboxIfPrimitive)
        loader <- frame.classLoader()
        result <- tree.op.evaluate(rhs, loader)
      } yield result

    private def invoke(tree: InstanceMethodTree): Safe[JdiValue] =
      for {
        qualValue <- evaluate(tree.qual)
        argsValues <- tree.args.map(evaluate).traverse
        loader <- frame.classLoader()
        argsBoxedIfNeeded <- loader.boxUnboxOnNeed(tree.method.argumentTypes(), argsValues)
        result <- qualValue.asObject.invoke(tree.method, argsBoxedIfNeeded)
      } yield result

    /* -------------------------------------------------------------------------- */
    /*                              Module evaluation                             */
    /* -------------------------------------------------------------------------- */
    /**
     * Returns the instance of the module. A call to the accessor method initialize it if needed.
     *
     * Returns a [[Safe[Failure[_]]]] if there are no instances and accessor method can't be found
     *
     * @param tree the module to evaluate
     * @param of the potential parent of the module
     * @return an instance of the module in a Safe
     */
    private def evaluateModule(tree: ModuleTree, of: Option[Safe[JdiValue]]): Safe[JdiValue] = {
      val module = Safe(JdiObject(tree.`type`.instances(1).get(0), frame.thread))
      (module, of) match {
        case (Safe(Success(_)), _) => module
        case (Safe(Failure(e: ArrayIndexOutOfBoundsException)), Some(qual)) =>
          for {
            ofValue <- qual
            loader <- frame.classLoader()
            initMethodName <- Safe(getLastInnerType(tree.`type`.name()).get)
            instance <- ofValue.asObject.invoke(initMethodName, Seq.empty)
          } yield instance
        case _ => Safe(Failure(new Exception("Can't evaluate or initialize module")))
      }
    }

    /* -------------------------------------------------------------------------- */
    /*                                Instantiation                               */
    /* -------------------------------------------------------------------------- */
    /**
     * Returns an instance of the class. Arguments are boxed/unboxed on need
     */
    private def instantiate(tree: NewInstanceTree): Safe[JdiObject] =
      for {
        args <- tree.args.map(evaluate).traverse
        loader <- frame.classLoader()
        boxedUnboxedArgs <- loader.boxUnboxOnNeed(tree.method.argumentTypes(), args)
        instance <- JdiClass(tree.`type`, frame.thread).newInstance(tree.method, boxedUnboxedArgs)
      } yield instance
  }

  /* -------------------------------------------------------------------------- */

  private object RuntimeValidation {
    def validate(expression: Stat): Validation[RuntimeEvaluationTree] = expression match {
      case value: Term.Name => validateName(value, thisTree)
      case _: Term.This => thisTree
      case sup: Term.Super => Recoverable("Super not (yet) supported at runtime")
      case _: Term.Apply | _: Term.ApplyInfix | _: Term.ApplyUnary => validateMethod(extractCall(expression))
      case select: Term.Select => validateSelect(select)
      case lit: Lit => validateLiteral(lit)
      case instance: Term.New => validateNew(instance)
      case _ => Recoverable("Expression not supported at runtime")
    }

    /**
     * Validates an expression, with access to class lookup. Its result must be contained in an [[EvaluationTree]]
     *
     * @param expression
     * @return a [[ValidationTree]] of the expression
     */
    private def validateWithClass(expression: Stat): Validation[RuntimeTree] =
      expression match {
        case value: Term.Name => validateName(value, thisTree).orElse(validateClass(value.value, None))
        case select: Term.Select => validateInnerSelect(select)
        case _ => validate(expression)
      }

    /**
     * Returns a ValidationTree of the [[Term.Select]] nested in another [[Term.Select]]. Provides access to [[ClassTree]], so it mustn't be used directly and must be wrapped in an [[EvaluationTree]]
     *
     * @param select
     * @return a [[ValidationTree]] of the qualifier
     */
    private def validateInnerSelect(select: Term.Select): Validation[RuntimeTree] =
      select.qual match {
        case s: Term.Select =>
          validateInnerSelect(s)
            .flatMap(getNestedSelectedTerm(_, s.name.value))
            .flatMap(getNestedSelectedTerm(_, select.name.value))
        case _ =>
          validateWithClass(select.qual).flatMap(getNestedSelectedTerm(_, select.name.value))
      }

    /* -------------------------------------------------------------------------- */
    /*                             Literal validation                             */
    /* -------------------------------------------------------------------------- */
    private def validateLiteral(lit: Lit): Validation[LiteralTree] =
      frame.classLoader().map(loader => LiteralTree(MatchingMethods.fromLitToValue(lit, loader))).getResult match {
        case Success(value) => value
        case Failure(e) => Unrecoverable(e)
      }

    /* -------------------------------------------------------------------------- */
    /*                               Name validation                              */
    /* -------------------------------------------------------------------------- */
    private def varTreeByName(name: String): Validation[RuntimeEvaluationTree] =
      Validation.fromOption(frame.variableByName(name)).map(v => LocalVarTree(name, v.`type`))

    /**
     * Returns a [[FieldTree]] if name is a field, or a [[ModuleTree]] if it is a module (in Scala 3 we can access inner modules by a field). Load the field return type on need.
     *
     * The conversion to a [[StaticFieldTree]] is automatic if the field is static.
     *
     * @param of
     * @param name
     * @return a [[RuntimeEvaluationTree]] representing the field or module
     */
    private def fieldTreeByName(
        of: Validation[RuntimeTree],
        name: String
    ): Validation[RuntimeEvaluationTree] =
      ifReference(of).flatMap { ref =>
        for {
          field <- Validation(ref.fieldByName(name))
          _ = loadClassOnNeed(field, frame)
          finalField <- field.`type` match {
            case Module(module) => ModuleTree(module, of.toOption)
            case _ => Valid(toStaticIfNeeded(field, of.get))
          }
        } yield finalField
      }

    /**
     * Returns a [[MethodTree]] if name is a method. Load the method return type on need.
     *
     * The conversion to a [[StaticMethodTree]] is automatic if the method is static.
     *
     * @param of
     * @param name
     * @return
     */
    private def zeroArgMethodTreeByName(
        of: Validation[RuntimeTree],
        name: String
    ): Validation[RuntimeEvaluationTree] =
      ifReference(of)
        .flatMap { methodsByNameAndArgs(_, name, List.empty, frame) }
        .map(toStaticIfNeeded(_, List.empty, of.get))

    /**
     * Returns a [[ModuleTree]], if name is a (nested) module
     *
     * If name is in the companion class, returns an [[Invalid]] to avoid static access to instance members
     *
     * @param name
     * @param of an option representing the qualifier of the module. It helps resolving name conflicts by selecting the most suitable one
     * @return a [[ModuleTree]] representing the module
     */
    private def validateModule(name: String, of: Option[RuntimeTree]): Validation[ModuleTree] = {
      val moduleName = if (name.endsWith("$")) name else name + "$"
      searchAllClassesFor(moduleName, of.map(_.`type`.name()), frame).flatMap { cls =>
        val isInClass = loadClass(removeLastInnerTypeFromFQCN(cls.name()), frame)
          .withFilterNot { _.cls.methodsByName(moduleName.stripSuffix("$")).isEmpty() }

        // TODO: understand why I can't merge n°1 and n°3
        (isInClass, cls, of) match {
          case (Safe(Success(_)), _, Instance(instance)) => ModuleTree(cls, Some(instance))
          case (_, Module(module), _) => ModuleTree(module, of)
          case (_, _, Instance(instance)) => ModuleTree(cls, Some(instance))
          case _ => Recoverable(s"Cannot access module $cls")
        }
      }
    }

    /**
     * Returns a [[ClassTree]]] if the name is a (nested class). Fails when accessing a non-static class from a static context (e.g. when of is a [[ClassTree]])
     *
     * @param name
     * @param of the potential parent of the class, can be another [[ClassTree]]
     * @return a [[ClassTree]] representing the class
     */
    private def validateClass(name: String, of: Option[RuntimeTree]): Validation[ClassTree] =
      searchAllClassesFor(name.stripSuffix("$"), of.map(_.`type`.name()), frame)
        .flatMap { cls =>
          (cls, of) match {
            case (cls, Some(_: RuntimeEvaluationTree) | None) => Valid(ClassTree(cls))
            case (cls, Some(_: ClassTree)) =>
              if (cls.isStatic()) Valid(ClassTree(cls))
              else Unrecoverable(s"Cannot access non-static class ${cls.name()}")
          }
        }

    private def validateName(
        value: Term.Name,
        of: Validation[RuntimeTree]
    ): Validation[RuntimeEvaluationTree] = {
      val name = NameTransformer.encode(value.value)
      varTreeByName(name)
        .orElse(fieldTreeByName(of, name))
        .orElse(zeroArgMethodTreeByName(of, name))
        .recoverWith(validateModule(name, of.toOption))
        .recoverWith {
          of
            .flatMap(findOuter(_, frame))
            .flatMap(outer => validateName(value, Valid(outer)))
        }
    }

    /* -------------------------------------------------------------------------- */
    /*                              Apply validation                              */
    /* -------------------------------------------------------------------------- */
    private def validateApplyCall(
        moduleName: String,
        on: RuntimeTree,
        args: Seq[RuntimeEvaluationTree]
    ): Validation[MethodTree] =
      for {
        module <- validateModule(moduleName, Some(on)).orElse(validateClass(moduleName, Some(on)))
        applyCall <- methodsByNameAndArgs(module.`type`, "apply", args.map(_.`type`), frame)
      } yield toStaticIfNeeded(applyCall, args, module)

    /**
     * "Unwrap" the apply method hidden by a 0-arg method returning the module
     *
     * @param ref the reference type to which the 0-arg method belongs
     * @param on the tree on which is called the 0-arg method
     * @param name the name of the 0-arg method
     * @param args the argument of the apply method
     * @return
     */
    private def validateImplicitApplyCall(
        ref: ReferenceType,
        on: RuntimeTree,
        name: String,
        args: Seq[RuntimeEvaluationTree]
    ): Validation[MethodTree] =
      on match {
        case _: RuntimeValidationTree => Recoverable("Cannot apply instance method on a ValidationTree")
        case eval: RuntimeEvaluationTree =>
          for {
            method <- methodsByNameAndArgs(ref, name, List.empty, frame)
            methodTree = InstanceMethodTree(method, Seq.empty, eval)
            apply <- validateApplyCall(method.returnTypeName(), methodTree, args)
          } yield apply
      }

    /**
     * Returns a [[MethodTree]] representing the method called.
     *
     * Method resolution is done by name and arguments type
     *
     * Look for same name methods or apply calls (explicit or implicit)
     *
     * @param tree
     * @param name
     * @param args
     * @return a [[MethodTree]] representing the method
     */
    private def findMethod(
        tree: RuntimeTree,
        name: String,
        args: Seq[RuntimeEvaluationTree]
    ): Validation[MethodTree] =
      tree.`type` match {
        case ref: ReferenceType =>
          methodsByNameAndArgs(ref, name, args.map(_.`type`), frame)
            .map(toStaticIfNeeded(_, args, tree))
            .orElse(validateApplyCall(name, tree, args))
            .orElse(validateImplicitApplyCall(ref, tree, name, args))
            .orElse(findOuter(tree, frame).flatMap(findMethod(_, name, args)))
        case t => illegalAccess(t, "ReferenceType")
      }

    /**
     * Returns a [[MethodTree]] representing the method called if it is on a reference.
     *
     * Returns a [[PrimitiveBinaryOpTree]] or [[PrimitiveUnaryOpTree]] if the method is primitive
     *
     * @param call the standardize call
     * @return a [[RuntimeEvaluationTree]] representing the call
     */
    private def validateMethod(call: Call): Validation[RuntimeEvaluationTree] = {
      lazy val preparedCall = call.fun match {
        case select: Term.Select =>
          PreparedCall(validateWithClass(select.qual), select.name.value)
        case name: Term.Name => PreparedCall(thisTree, name.value)
      }

      val lhs = preparedCall.qual
      def unary(l: RuntimeTree, name: String) =
        l match {
          case ret: RuntimeEvaluationTree => RuntimeUnaryOp(ret, name).map(PrimitiveUnaryOpTree(ret, _))
          case _ => Recoverable(s"Primitive operation operand must be evaluable")
        }
      def binary(l: RuntimeTree, args: Seq[RuntimeEvaluationTree], name: String) =
        (l, args) match {
          case (ret: RuntimeEvaluationTree, Seq(right)) =>
            RuntimeBinaryOp(ret, right, name).map(PrimitiveBinaryOpTree(ret, right, _))
          case _ => Recoverable(s"Primitive operation operand must be evaluable")
        }

      for {
        lhs <- lhs
        args <- call.argClause.map(validate).traverse
        methodTree <- unary(lhs, preparedCall.name)
          .orElse(binary(lhs, args, preparedCall.name))
          .orElse(findMethod(lhs, preparedCall.name, args))
      } yield methodTree
    }

    /* -------------------------------------------------------------------------- */
    /*                              Select validation                             */
    /* -------------------------------------------------------------------------- */
    /* Only look for evaluable terms (field, methods, module) */
    private def getSelectedTerm(of: RuntimeTree, name: String): Validation[RuntimeEvaluationTree] =
      fieldTreeByName(Valid(of), name)
        .orElse(zeroArgMethodTreeByName(Valid(of), name))
        .orElse(validateModule(name, Some(of)))

    /**
     * Returns a [[RuntimeValidationTree]], representing any kind of term. Allows to access to inner classes
     *
     * This method is only valid in the context of chained select and its result must be contained in a [[RuntimeEvaluationTree]] for it cannot be evaluated standalone
     *
     * @param of
     * @param name
     * @return a [[RuntimeValidationTree]] representing the term
     */
    private def getNestedSelectedTerm(of: RuntimeTree, name: String) = {
      getSelectedTerm(of, name).orElse(validateClass(name, Some(of)))
    }

    private def validateSelect(select: Term.Select): Validation[RuntimeEvaluationTree] =
      select.qual match {
        case s: Term.Select =>
          validateWithClass(s).flatMap(getSelectedTerm(_, select.name.value))
        case _ => validateWithClass(select.qual).flatMap(getSelectedTerm(_, select.name.value))
      }

    /* -------------------------------------------------------------------------- */
    /*                               New validation                               */
    /* -------------------------------------------------------------------------- */
    private def validateNew(newValue: Term.New): Validation[RuntimeEvaluationTree] = {
      val name = newValue.init.tpe.toString
      val argClauses = newValue.init.argClauses

      for {
        args <- argClauses.flatMap(_.map(validate(_))).traverse
        cls <- searchAllClassesFor(name, None, frame)
        method <- methodsByNameAndArgs(cls, "<init>", args.map(_.`type`), frame, encode = false)
      } yield NewInstanceTree(method, args)
    }
  }
}

/* -------------------------------------------------------------------------- */

private object MatchingMethods {
  def fromLitToValue(literal: Lit, classLoader: JdiClassLoader): (Safe[Any], Type) = {
    val tpe = classLoader
      .mirrorOfLiteral(literal.value)
      .map(_.value.`type`)
      .getResult
      .get

    (Safe(literal.value), tpe)
  }

  /* -------------------------------------------------------------------------- */
  /*                                Method lookup                               */
  /* -------------------------------------------------------------------------- */
  private def argsMatch(method: Method, args: Seq[Type], frame: JdiFrame): Boolean =
    method.argumentTypeNames().size() == args.size && areAssignableFrom(method, args, frame)

  /**
   * Look for a method with the given name and arguments types, on the given reference type
   *
   * Encode the method name by default, but can be disabled for methods that are not Scala methods (such as Java <init> methods)
   *
   * If multiple methods are found, a [[Validation.Unrecoverable]] is returned
   *
   * Also, if the method return type is not loaded or prepared, it will be loaded and prepared
   *
   * @param ref the reference type on which to look for the method
   * @param funName the name of the method
   * @param args the arguments types of the method
   * @param frame the current frame
   * @param encode whether to encode the method name or not
   * @return the method, wrapped in a [[Validation]], with its return type loaded and prepared
   */
  def methodsByNameAndArgs(
      ref: ReferenceType,
      funName: String,
      args: Seq[Type],
      frame: JdiFrame,
      encode: Boolean = true
  ): Validation[Method] = {
    val candidates: Seq[Method] = ref
      .methodsByName { if (encode) NameTransformer.encode(funName) else funName }
      .asScalaSeq
      .filter { method => !method.isPrivate && argsMatch(method, args, frame) }
      .toSeq

    val finalCandidates = candidates.size match {
      case 0 | 1 => candidates
      case _ => candidates.filterNot(_.isBridge())
    }

    finalCandidates
      .toValidation(s"Cannot find methods $funName with args types $args on $ref")
      .map(loadClassOnNeed(_, frame))
  }

  /* -------------------------------------------------------------------------- */
  /*                                Type checker                                */
  /* -------------------------------------------------------------------------- */
  def isAssignableFrom(got: Type, expected: Type, frame: JdiFrame): Boolean = {
    def referenceTypesMatch(got: ReferenceType, expected: ReferenceType) = {
      val assignableFrom = expected.classObject().referenceType().methodsByName("isAssignableFrom").get(0)
      val params = Seq(got.classObject()).asJavaList
      expected.classObject
        .invokeMethod(frame.thread, assignableFrom, params, ObjectReference.INVOKE_SINGLE_THREADED)
        .asInstanceOf[BooleanValue]
        .value()
    }

    (got, expected) match {
      case (g: ArrayType, at: ArrayType) => isAssignableFrom(g.componentType, at.componentType, frame)
      case (g: PrimitiveType, pt: PrimitiveType) => got.equals(pt)
      case (g: ReferenceType, ref: ReferenceType) => referenceTypesMatch(g, ref)
      case (_: VoidType, _: VoidType) => true

      case (g: ClassType, pt: PrimitiveType) =>
        isAssignableFrom(g, frame.getPrimitiveBoxedClass(pt), frame)
      case (g: PrimitiveType, ct: ReferenceType) =>
        isAssignableFrom(frame.getPrimitiveBoxedClass(g), ct, frame)

      case _ => false
    }
  }

  def areAssignableFrom(method: Method, args: Seq[Type], frame: JdiFrame): Boolean =
    method.argumentTypes().asScalaSeq.zip(args).forall { case (expected, got) =>
      isAssignableFrom(got, expected, frame)
    }
}

/* --------------------------------- Helpers -------------------------------- */

private object Helpers {
  def illegalAccess(x: Any, typeName: String) = Unrecoverable {
    new ClassCastException(s"Cannot cast $x to $typeName")
  }

  /* -------------------------------------------------------------------------- */
  /*                              Looking for $outer                             */
  /* -------------------------------------------------------------------------- */
  def findOuter(tree: RuntimeTree, frame: JdiFrame): Validation[OuterTree] = {
    def outerLookup(ref: ReferenceType) = Validation(ref.fieldByName("$outer")).map(_.`type`()).orElse {
      loadClass(removeLastInnerTypeFromFQCN(ref.name()) + "$", frame) match {
        case Safe(Success(Module(mod: ClassType))) => Valid(mod)
        case _ => Recoverable(s"Cannot find $$outer for $ref")
      }
    }

    for {
      ref <- ifReference(tree)
      outer <- outerLookup(ref)
      outerTree <- OuterTree(tree, outer)
    } yield outerTree
  }

  /* -------------------------------------------------------------------------- */
  /*                               Useful patterns                              */
  /* -------------------------------------------------------------------------- */
  /* Extract reference if there is */
  def ifReference(tree: Validation[RuntimeTree]): Validation[ReferenceType] =
    tree match {
      case Invalid(e) => Unrecoverable(s"Invalid reference: $e")
      case _ => ifReference(tree.get)
    }

  def ifReference(tree: RuntimeTree): Validation[ReferenceType] =
    tree match {
      case ReferenceTree(ref) => Valid(ref)
      case t => illegalAccess(t, "ReferenceType")
    }

  /* Standardize method calls */
  def extractCall(apply: Stat): Call =
    apply match {
      case apply: Term.Apply => Call(apply.fun, apply.argClause)
      case ColonEndingInfix(apply) => Call(Term.Select(apply.argClause.head, apply.op), List(apply.lhs))
      case apply: Term.ApplyInfix => Call(Term.Select(apply.lhs, apply.op), apply.argClause)
      case apply: Term.ApplyUnary => Call(Term.Select(apply.arg, Term.Name("unary_" + apply.op)), List.empty)
    }

  /* -------------------------------------------------------------------------- */
  /*                           Last nested types regex                          */
  /* -------------------------------------------------------------------------- */
  def getLastInnerType(className: String): Option[String] = {
    val pattern = """(.+\$)([^$]+)$""".r
    className.stripSuffix("$") match {
      case pattern(_, innerType) => Some(innerType)
      case _ => None
    }
  }

  def removeLastInnerTypeFromFQCN(className: String): String = {
    val pattern = """(.+)\$[\w]+\${0,1}$""".r
    className match {
      case pattern(baseName) => baseName
      case _ => className
    }
  }

  /* -------------------------------------------------------------------------- */
  /*                  Transformation to static or instance tree                 */
  /* -------------------------------------------------------------------------- */
  def toStaticIfNeeded(field: Field, on: RuntimeTree): FieldTree = on match {
    case cls: ClassTree => StaticFieldTree(field, cls.`type`)
    case eval: RuntimeEvaluationTree => InstanceFieldTree(field, eval)
  }

  def toStaticIfNeeded(
      method: Method,
      args: Seq[RuntimeEvaluationTree],
      on: RuntimeTree
  ): MethodTree = on match {
    case cls: ClassTree => StaticMethodTree(method, args, cls.`type`)
    case eval: RuntimeEvaluationTree => InstanceMethodTree(method, args, eval)
  }

  /* -------------------------------------------------------------------------- */
  /*                                Class helpers                               */
  /* -------------------------------------------------------------------------- */
  def loadClass(name: String, frame: JdiFrame): Safe[JdiClass] =
    frame.classLoader().flatMap(_.loadClass(name))

  def checkClass(tpe: => Type)(name: String, frame: JdiFrame) = Try(tpe) match {
    case Failure(_: ClassNotLoadedException) => loadClass(name, frame).getResult.map(_.cls)
    case Success(value: ClassType) if !value.isPrepared => loadClass(name, frame).getResult.map(_.cls)
    case result => result
  }

  def loadClassOnNeed[T <: TypeComponent](tc: T, frame: JdiFrame): T = {
    checkClass(tc.`type`)(tc.typeName, frame)
    tc
  }

  def searchAllClassesFor(name: String, in: Option[String], frame: JdiFrame): Validation[ClassType] = {
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
        case _ => false
      }

    def candidates =
      frame
        .current()
        .virtualMachine
        .allClasses()
        .asScalaSeq
        .filter { cls => cls.name() == name || nameEndMatch(cls.name()) }

    def finalCandidates =
      candidates.size match {
        case 0 =>
          val topLevelClassName = frame.thisObject
            .map(_.reference.referenceType.name)
            .map(_.split('.').init.mkString(".") + "." + name)
            .getOrElse("")
          loadClass(fullName, frame).getResult
            .orElse { loadClass(topLevelClassName, frame).getResult }
            .map(_.cls)
            .toSeq
        case 1 => candidates
        case _ => candidates.filter(_.name() == fullName)
      }

    finalCandidates
      .toValidation(s"Cannot find module/class $name, has it been loaded ?")
      .map { cls => checkClass(cls)(cls.name(), frame).get.asInstanceOf[ClassType] }
  }

}
