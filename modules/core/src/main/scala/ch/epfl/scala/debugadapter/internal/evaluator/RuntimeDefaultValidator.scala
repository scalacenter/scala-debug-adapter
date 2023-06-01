package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import com.sun.jdi._
import scala.meta.{Type => _, _}
import scala.meta.trees.*
import scala.meta.parsers.*
import Helpers.*
import RuntimeEvaluatorExtractors.*
import scala.util.Failure
import scala.util.Success

case class Call(fun: Term, argClause: Term.ArgClause)
case class PreparedCall(qual: Validation[RuntimeTree], name: String)

class RuntimeDefaultValidator(val frame: JdiFrame, val logger: Logger) extends RuntimeValidator {
  protected def parse(expression: String): Validation[Stat] =
    expression.parse[Stat] match {
      case err: Parsed.Error => Fatal(err.details)
      case Parsed.Success(tree) => Valid(tree)
      case _: Parsed.Success[?] =>
        Fatal(new Exception("Parsed expression is not a statement"))
    }

  def validate(expression: String): Validation[RuntimeEvaluableTree] =
    parse(expression).flatMap(validate)

  def validate(expression: Stat): Validation[RuntimeEvaluableTree] =
    expression match {
      case value: Term.Name => validateName(value, thisTree)
      case _: Term.This => thisTree
      case sup: Term.Super => Recoverable("Super not (yet) supported at runtime")
      case _: Term.Apply | _: Term.ApplyInfix | _: Term.ApplyUnary => validateMethod(extractCall(expression))
      case select: Term.Select => validateSelect(select)
      case lit: Lit => validateLiteral(lit)
      case instance: Term.New => validateNew(instance)
      case _ => Recoverable("Expression not supported at runtime")
    }

  protected def validateWithClass(expression: Stat): Validation[RuntimeTree] =
    expression match {
      case value: Term.Name => validateName(value, thisTree).orElse(validateClass(value.value, thisTree.toOption))
      case select: Term.Select => validateInnerSelect(select)
      case _ => validate(expression)
    }

  protected def validateInnerSelect(select: Term.Select): Validation[RuntimeTree] = {
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
  def validateLiteral(lit: Lit): Validation[LiteralTree] =
    frame.classLoader().map(loader => LiteralTree(fromLitToValue(lit, loader))).getResult match {
      case Success(value) => value
      case Failure(e) => Fatal(e)
    }

  /* -------------------------------------------------------------------------- */
  /*                               This validation                              */
  /* -------------------------------------------------------------------------- */
  lazy val thisTree: Validation[RuntimeEvaluableTree] =
    Validation.fromOption {
      frame.thisObject
        .map { ths => ThisTree(ths.reference.referenceType().asInstanceOf[ClassType]) }
    }

  /* -------------------------------------------------------------------------- */
  /*                               Name validation                              */
  /* -------------------------------------------------------------------------- */
  def localVarTreeByName(name: String): Validation[RuntimeEvaluableTree] =
    Validation
      .fromOption(frame.variableByName(name))
      .filter(_.`type`.name() != "scala.Function0", runtimeFatal = true)
      .map(v => LocalVarTree(name, v.`type`))

  def fieldTreeByName(
      of: Validation[RuntimeTree],
      name: String
  ): Validation[RuntimeEvaluableTree] =
    ifReference(of).flatMap { ref =>
      for {
        field <- Validation(ref.fieldByName(name))
        _ = loadClassOnNeed(field, frame)
        fieldTree <- toStaticIfNeeded(field, of.get)
      } yield fieldTree
    }

  def zeroArgMethodTreeByName(
      of: Validation[RuntimeTree],
      name: String
  ): Validation[RuntimeEvaluableTree] =
    ifReference(of)
      .flatMap { methodsByNameAndArgs(_, name, List.empty, frame) }
      .flatMap(toStaticIfNeeded(_, List.empty, of.get))

  def validateModule(name: String, of: Option[RuntimeTree]): Validation[RuntimeEvaluableTree] = {
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
        case (_, _, Some(instance: RuntimeEvaluableTree)) =>
          if (moduleCls.name().startsWith(instance.`type`.name()))
            Valid(NestedModuleTree(moduleCls, instance))
          else Recoverable(s"Cannot access module $moduleCls from ${instance.`type`.name()}")
        case _ => Recoverable(s"Cannot access module $moduleCls")
      }
    }
  }

  def validateClass(name: String, of: Option[RuntimeTree]): Validation[ClassTree] =
    searchAllClassesFor(name.stripSuffix("$"), of.map(_.`type`.name()), frame)
      .flatMap { cls =>
        of match {
          case Some(_: RuntimeEvaluableTree) | None => Valid(ClassTree(cls))
          case Some(_: ClassTree) =>
            if (cls.isStatic()) Valid(ClassTree(cls))
            else CompilerRecoverable(s"Cannot access non-static class ${cls.name()}")
          // TODO: check if this could not be Fatal
        }
      }
      .orElse {
        of match {
          case None => Recoverable(s"Cannot find class $name")
          case Some(value) => findOuter(value, frame).flatMap(outer => validateClass(name, Some(outer)))
        }
      }

  def validateName(
      value: Term.Name,
      of: Validation[RuntimeTree]
  ): Validation[RuntimeEvaluableTree] = {
    val name = NameTransformer.encode(value.value)
    localVarTreeByName(name)
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
  def validateApplyCall(
      moduleName: String,
      on: RuntimeTree,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[MethodTree] =
    for {
      module <- validateModule(moduleName, Some(on)).orElse(validateClass(moduleName, Some(on)))
      applyCall <- ifReference(module).flatMap(methodsByNameAndArgs(_, "apply", args.map(_.`type`), frame))
      methodTree <- toStaticIfNeeded(applyCall, args, module)
    } yield methodTree

  def validateImplicitApplyCall(
      on: RuntimeTree,
      name: String,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[MethodTree] =
    ifReference(on).flatMap { ref =>
      on match {
        case _: RuntimeValidationTree => Recoverable("Cannot apply instance method on a ValidationTree")
        case eval: RuntimeEvaluableTree =>
          for {
            method <- methodsByNameAndArgs(ref, name, List.empty, frame)
            methodTree = InstanceMethodTree(method, Seq.empty, eval)
            apply <- validateApplyCall(method.returnTypeName(), methodTree, args)
          } yield apply
      }
    }

  def findMethod(
      tree: RuntimeTree,
      name: String,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[MethodTree] =
    ifReference(tree).flatMap { ref =>
      validateApplyCall(name, tree, args)
        .orElse {
          methodsByNameAndArgs(ref, name, args.map(_.`type`), frame)
            .flatMap(toStaticIfNeeded(_, args, tree))
        }
        .orElse(validateImplicitApplyCall(tree, name, args))
        .orElse(findOuter(tree, frame).flatMap(findMethod(_, name, args)))
    }

  def validateMethod(call: Call): Validation[RuntimeEvaluableTree] = {
    lazy val preparedCall = call.fun match {
      case select: Term.Select =>
        PreparedCall(validateWithClass(select.qual), select.name.value)
      case name: Term.Name => PreparedCall(thisTree, name.value)
    }

    for {
      args <- call.argClause.map(validate).traverse
      lhs <- preparedCall.qual
      methodTree <-
        PrimitiveUnaryOpTree(lhs, preparedCall.name)
          .orElse { PrimitiveBinaryOpTree(lhs, args, preparedCall.name) }
          .orElse { findMethod(lhs, preparedCall.name, args) }
    } yield methodTree
  }

  /* -------------------------------------------------------------------------- */
  /*                              Select validation                             */
  /* -------------------------------------------------------------------------- */
  def getSelectedTerm(of: RuntimeTree, name: String): Validation[RuntimeEvaluableTree] =
    fieldTreeByName(Valid(of), name)
      .orElse(zeroArgMethodTreeByName(Valid(of), name))
      .orElse(validateModule(name, Some(of)))
      .orElse(findOuter(of, frame).flatMap(getSelectedTerm(_, name)))

  def validateSelect(select: Term.Select): Validation[RuntimeEvaluableTree] =
    validateWithClass(select.qual).flatMap(getSelectedTerm(_, select.name.value))

  /* -------------------------------------------------------------------------- */
  /*                               New validation                               */
  /* -------------------------------------------------------------------------- */
  def validateNew(newValue: Term.New): Validation[RuntimeEvaluableTree] = {
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
