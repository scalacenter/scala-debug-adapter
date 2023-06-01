package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import com.sun.jdi._
import scala.meta.{Type => _, _}
import scala.meta.parsers.*
import scala.meta.trees.*
import Helpers.*
import RuntimeEvaluatorExtractors.*
import scala.util.Failure
import scala.util.Success

private case class Call(fun: Term, argClause: Term.ArgClause)
private case class PreparedCall(qual: Validation[RuntimeTree], name: String)

class RuntimeEvaluation private (
    evaluator: RuntimeEvaluator,
    validator: RuntimeValidator,
    logger: Logger
) {
  private def parse(expression: String): Validation[Stat] =
    expression.parse[Stat] match {
      case err: Parsed.Error => Fatal(err.details)
      case Parsed.Success(tree) => Valid(tree)
      case _: Parsed.Success[?] =>
        Fatal(new Exception("Parsed expression is not a statement"))
    }

  def validate(expression: String): Validation[RuntimeEvaluationTree] =
    parse(expression).flatMap(validator.validate)

  def evaluate(expression: RuntimeEvaluationTree): Safe[JdiValue] =
    evaluator.evaluate(expression).map(_.derefIfRef)
}

object RuntimeEvaluation {
  def apply(frame: JdiFrame, logger: Logger): RuntimeEvaluation =
    new RuntimeEvaluation(
      new RuntimeEvaluator(frame, logger),
      new RuntimeValidator(frame, logger),
      logger
    )
}

/* -------------------------------------------------------------------------- */

class RuntimeEvaluator(frame: JdiFrame, logger: Logger) {
  def evaluate(stat: RuntimeEvaluationTree): Safe[JdiValue] =
    stat match {
      case LocalVarTree(varName, _) => Safe.successful(frame.variableByName(varName).map(frame.variableValue).get)
      case primitive: PrimitiveBinaryOpTree => invokePrimitive(primitive)
      case primitive: PrimitiveUnaryOpTree => invokePrimitive(primitive)
      case module: ModuleTree => evaluateModule(module)
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
    tree match {
      case OuterModuleTree(module) => evaluateModule(module)
      case outerClass: OuterClassTree =>
        evaluate(outerClass.inner).map(_.asObject.getField("$outer"))
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
  private def evaluateModule(tree: ModuleTree): Safe[JdiValue] =
    tree match {
      case TopLevelModuleTree(mod) => Safe(JdiObject(mod.instances(1).get(0), frame.thread))
      case NestedModuleTree(mod, of) =>
        for {
          ofValue <- evaluate(of)
          loader <- frame.classLoader()
          initMethodName <- Safe(getLastInnerType(tree.`type`.name()).get)
          instance <- ofValue.value.`type` match {
            case module if module == mod => Safe(ofValue)
            case _ => ofValue.asObject.invoke(initMethodName, Seq.empty)
          }
        } yield instance
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

class RuntimeValidator(frame: JdiFrame, logger: Logger) {
  private val thisTree =
    Validation.fromOption {
      frame.thisObject
        .map { ths => ThisTree(ths.reference.referenceType().asInstanceOf[ClassType]) }
    }

  def validate(expression: Stat): Validation[RuntimeEvaluationTree] = {
    val validation = expression match {
      case value: Term.Name => validateName(value, thisTree)
      case _: Term.This => thisTree
      case sup: Term.Super => Recoverable("Super not (yet) supported at runtime")
      case _: Term.Apply | _: Term.ApplyInfix | _: Term.ApplyUnary => validateMethod(extractCall(expression))
      case select: Term.Select => validateSelect(select)
      case lit: Lit => validateLiteral(lit)
      case instance: Term.New => validateNew(instance)
      case _ => Recoverable("Expression not supported at runtime")
    }
    validation.filterNot(_.`type`.name() == "scala.Function0", runtimeFatal = true)
  }

  /**
   * Validates an expression, with access to class lookup. Its result must be contained in an [[EvaluationTree]]
   *
   * @param expression
   * @return a [[ValidationTree]] of the expression
   */
  private def validateWithClass(expression: Stat): Validation[RuntimeTree] =
    expression match {
      case value: Term.Name => validateName(value, thisTree).orElse(validateClass(value.value, thisTree.toOption))
      case select: Term.Select => validateInnerSelect(select)
      case _ => validate(expression)
    }

  /**
   * Returns a ValidationTree of the [[Term.Select]] nested in another [[Term.Select]]. Provides access to [[ClassTree]], so it mustn't be used directly and must be wrapped in an [[EvaluationTree]]
   *
   * @param select
   * @return a [[ValidationTree]] of the qualifier
   */
  private def validateInnerSelect(select: Term.Select): Validation[RuntimeTree] = {
    def getNestedSelectedTerm(of: RuntimeTree, name: String) =
      getSelectedTerm(of, name).orElse(validateClass(name, Some(of)))
    select.qual match {
      case s: Term.Select =>
        validateInnerSelect(s)
          .flatMap(getNestedSelectedTerm(_, s.name.value))
          .flatMap(getNestedSelectedTerm(_, select.name.value))
      case _ =>
        validateWithClass(select.qual).flatMap(getNestedSelectedTerm(_, select.name.value))
    }
  }

  /* -------------------------------------------------------------------------- */
  /*                             Literal validation                             */
  /* -------------------------------------------------------------------------- */
  private def validateLiteral(lit: Lit): Validation[LiteralTree] =
    frame.classLoader().map(loader => LiteralTree(fromLitToValue(lit, loader))).getResult match {
      case Success(value) => value
      case Failure(e) => Fatal(e)
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
      } yield field.`type` match {
        case Module(module) => TopLevelModuleTree(module)
        case _ => toStaticIfNeeded(field, of.get)
      }
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
   * Should the module be a nested module, then the qualifier must be an instance of the outer module (to avoid accessing a field from a static context)
   *
   * @param name
   * @param of an option representing the qualifier of the module. It helps resolving name conflicts by selecting the most suitable one
   * @return a [[ModuleTree]] representing the module
   */
  private def validateModule(name: String, of: Option[RuntimeTree]): Validation[ModuleTree] = {
    val moduleName = if (name.endsWith("$")) name else name + "$"
    val ofName = of.map(_.`type`.name())
    searchAllClassesFor(moduleName, ofName, frame).flatMap { moduleCls =>
      val isInCompanionClass = ofName
        .filter(_.endsWith("$"))
        .map(n => loadClass(n.stripSuffix("$"), frame))
        .map {
          _.withFilterNot {
            _.cls.methodsByName(moduleName.stripSuffix("$")).isEmpty()
          }.getResult
        }

      (isInCompanionClass, moduleCls, of) match {
        case (Some(Success(cls: JdiClass)), _, _) =>
          CompilerRecoverable(s"Cannot access module ${name} from ${ofName}")
        case (_, Module(module), _) => Valid(TopLevelModuleTree(moduleCls))
        case (_, _, Some(instance: RuntimeEvaluationTree)) =>
          if (moduleCls.name().startsWith(instance.`type`.name()))
            Valid(NestedModuleTree(moduleCls, instance))
          else Recoverable(s"Cannot access module $moduleCls from ${instance.`type`.name()}")
        case _ => Recoverable(s"Cannot access module $moduleCls")
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
        of match {
          case Some(_: RuntimeEvaluationTree) | None => Valid(ClassTree(cls))
          case Some(_: ClassTree) =>
            if (cls.isStatic()) Valid(ClassTree(cls))
            else CompilerRecoverable(s"Cannot access non-static class ${cls.name()}")
          // Should be fatal, but I think in some cases using Fatal would break the evaluator (because of missing informations at runtime)
        }
      }
      .orElse {
        of match {
          case None => Recoverable(s"Cannot find class $name")
          case Some(value) => findOuter(value, frame).flatMap(outer => validateClass(name, Some(outer)))
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
      .orElse(validateModule(name, of.toOption))
      .orElse {
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
        validateApplyCall(name, tree, args)
          .orElse {
            methodsByNameAndArgs(ref, name, args.map(_.`type`), frame)
              .map(toStaticIfNeeded(_, args, tree))
          }
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

    for {
      lhs <- preparedCall.qual
      args <- call.argClause.map(validate).traverse
      methodTree <-
        PrimitiveUnaryOpTree(lhs, preparedCall.name)
          .orElse { PrimitiveBinaryOpTree(lhs, args, preparedCall.name) }
          .orElse { findMethod(lhs, preparedCall.name, args) }
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
      .orElse(findOuter(of, frame).flatMap(getSelectedTerm(_, name)))

  private def validateSelect(select: Term.Select): Validation[RuntimeEvaluationTree] =
    validateWithClass(select.qual).flatMap(getSelectedTerm(_, select.name.value))

  /* -------------------------------------------------------------------------- */
  /*                               New validation                               */
  /* -------------------------------------------------------------------------- */
  private def validateNew(newValue: Term.New): Validation[RuntimeEvaluationTree] = {
    val name = newValue.init.tpe.toString
    val argClauses = newValue.init.argClauses

    for {
      args <- argClauses.flatMap(_.map(validate(_))).traverse
      outerFqcn = thisTree.toOption.map(_.`type`.name())
      cls <- searchAllClassesFor(name, outerFqcn, frame)
      method <- methodsByNameAndArgs(cls, "<init>", args.map(_.`type`), frame, encode = false)
    } yield NewInstanceTree(method, args)
  }
}
