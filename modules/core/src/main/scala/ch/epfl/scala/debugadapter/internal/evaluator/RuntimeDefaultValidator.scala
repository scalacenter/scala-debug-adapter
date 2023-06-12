package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import com.sun.jdi._
import scala.meta.{Type => _, _}
import scala.meta.trees.*
import scala.meta.parsers.*
import RuntimeEvaluatorExtractors.*
import scala.util.Failure
import scala.util.Success

case class Call(fun: Term, argClause: Term.ArgClause)
case class PreparedCall(qual: Validation[RuntimeTree], name: Term.Name)

class RuntimeDefaultValidator(val frame: JdiFrame, val logger: Logger) extends RuntimeValidator {
  val helper = new RuntimeEvaluationHelpers(frame)
  import helper.*

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
      case Term.Select(qual, name) =>
        validateWithClass(qual)
          .flatMap { q => getSelectedTerm(q, name.value).orElse(validateClass(name.value, Some(q))) }
      case _ => validate(expression)
    }

  /* -------------------------------------------------------------------------- */
  /*                             Literal validation                             */
  /* -------------------------------------------------------------------------- */
  def validateLiteral(lit: Lit): Validation[LiteralTree] =
    frame.classLoader().map(loader => LiteralTree(fromLitToValue(lit, loader))).extract match {
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
    for {
      ref <- extractReferenceType(of)
      field <- Validation(ref.fieldByName(name))
      _ = loadClassOnNeed(field)
      fieldTree <- toStaticIfNeeded(field, of.get)
    } yield fieldTree

  def zeroArgMethodTreeByName(
      of: Validation[RuntimeTree],
      name: String
  ): Validation[RuntimeEvaluableTree] =
    of.flatMap(methodTreeByNameAndArgs(_, name, List.empty))

  def validateModule(name: String, of: Option[RuntimeTree]): Validation[RuntimeEvaluableTree] = {
    val moduleName = if (name.endsWith("$")) name else name + "$"
    val ofName = of.map(_.`type`.name())
    searchAllClassesFor(moduleName, ofName).flatMap { moduleCls =>
      val isInCompanionClass = ofName
        .filter(_.endsWith("$"))
        .map(n => loadClass(n.stripSuffix("$")))
        .map {
          _.withFilterNot {
            _.cls.methodsByName(moduleName.stripSuffix("$")).isEmpty()
          }.extract
        }

      (isInCompanionClass, moduleCls.`type`, of) match {
        case (Some(Success(cls: JdiClass)), _, _) =>
          CompilerRecoverable(s"Cannot access module ${name} from ${ofName}")
        case (_, Module(module), _) => Valid(TopLevelModuleTree(module))
        case (_, cls, Some(instance: RuntimeEvaluableTree)) =>
          if (cls.name().startsWith(instance.`type`.name()))
            Valid(NestedModuleTree(cls, instance))
          else Recoverable(s"Cannot access module $moduleCls from ${instance.`type`.name()}")
        case _ => Recoverable(s"Cannot access module $moduleCls")
      }
    }
  }

  def validateClass(name: String, of: Option[RuntimeTree]): Validation[ClassTree] =
    searchAllClassesFor(name.stripSuffix("$"), of.map(_.`type`.name()))
      .flatMap { cls =>
        of match {
          case Some(_: RuntimeEvaluableTree) | None => Valid(cls)
          case Some(ct: ClassTree) =>
            if (ct.`type`.isStatic()) Valid(ct)
            else CompilerRecoverable(s"Cannot access non-static class ${ct.`type`.name()}")
          // TODO: check if this could not be Fatal
        }
      }
      .orElse {
        of match {
          case None => Recoverable(s"Cannot find class $name")
          case Some(value) => findOuter(value).flatMap(outer => validateClass(name, Some(outer)))
        }
      }

  def validateName(
      value: Term.Name,
      of: Validation[RuntimeTree]
  ): Validation[RuntimeEvaluableTree] = {
    val name = NameTransformer.encode(value.value)
    localVarTreeByName(name)
      .orElse { fieldTreeByName(of, name) }
      .orElse { zeroArgMethodTreeByName(of, name) }
      .orElse { validateModule(name, of.toOption) }
      .orElse {
        of
          .flatMap(findOuter(_))
          .flatMap(outer => validateName(value, Valid(outer)))
      }
  }

  /* -------------------------------------------------------------------------- */
  /*                              Apply validation                              */
  /* -------------------------------------------------------------------------- */
  def validateArrayApply(on: RuntimeTree, args: Seq[RuntimeEvaluableTree]): Validation[MethodTree] =
    methodTreeByNameAndArgs(on, "apply", args)

  def validateApply(
      on: RuntimeTree,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[MethodTree] =
    methodTreeByNameAndArgs(on, "apply", args)

  def validateIndirectApply(
      on: RuntimeTree,
      name: Term.Name,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[MethodTree] =
    for {
      qualTree <- validateName(name, Valid(on)).orElse(validateClass(name.value, Some(on)))
      apply <- validateApply(qualTree, args)
    } yield apply

  def findMethod(
      tree: RuntimeTree,
      name: Term.Name,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[MethodTree] =
    methodTreeByNameAndArgs(tree, name.value, args)
      .orElse { validateIndirectApply(tree, name, args) }
      .orElse { validateApply(tree, args) }
      .orElse { findOuter(tree).flatMap(findMethod(_, name, args)) }

  def validateMethod(call: Call): Validation[RuntimeEvaluableTree] = {
    lazy val preparedCall = call.fun match {
      case select: Term.Select =>
        PreparedCall(validateWithClass(select.qual), select.name)
      case name: Term.Name => PreparedCall(thisTree, name)
    }

    for {
      args <- call.argClause.map(validate).traverse
      lhs <- preparedCall.qual
      methodTree <-
        PrimitiveUnaryOpTree(lhs, preparedCall.name.value)
          .orElse { PrimitiveBinaryOpTree(lhs, args, preparedCall.name.value) }
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
      .orElse(findOuter(of).flatMap(getSelectedTerm(_, name)))

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
      cls <- searchAllClassesFor(name, outerFqcn)
      method <- methodTreeByNameAndArgs(cls, "<init>", args, encode = false)
    } yield NewInstanceTree(method, args)
  }
}

object RuntimeDefaultValidator {
  def apply(frame: JdiFrame, logger: Logger) =
    new RuntimeDefaultValidator(frame, logger)
}
