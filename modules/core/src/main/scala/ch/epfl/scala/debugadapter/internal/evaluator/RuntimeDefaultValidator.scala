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
case class PreparedCall(qual: Validation[RuntimeTree], name: String)

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
      case value: Term.Name => validateName(value.value, thisTree)
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
      case value: Term.Name => validateName(value.value, thisTree).orElse(validateClass(value.value, thisTree))
      case Term.Select(qual, name) =>
        for {
          qual <- validateWithClass(qual)
          name <- validateName(name.value, Valid(qual)).orElse(validateClass(name.value, Valid(qual)))
        } yield name
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

  private def inCompanion(name: Option[String], moduleName: String) = name
    .filter(_.endsWith("$"))
    .map(n => loadClass(n.stripSuffix("$")))
    .map {
      _.withFilterNot {
        _.cls.methodsByName(moduleName.stripSuffix("$")).isEmpty()
      }.extract
    }

  def validateModule(name: String, of: Option[RuntimeTree]): Validation[RuntimeEvaluableTree] = {
    val moduleName = if (name.endsWith("$")) name else name + "$"
    val ofName = of.map(_.`type`.name())
    searchAllClassesFor(moduleName, ofName).flatMap { moduleCls =>
      val isInModule = inCompanion(ofName, moduleName)

      (isInModule, moduleCls.`type`, of) match {
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

  def validateClass(name: String, of: Validation[RuntimeTree]): Validation[ClassTree] =
    searchAllClassesFor(name.stripSuffix("$"), of.map(_.`type`.name()).toOption)
      .transform { cls =>
        (cls, of) match {
          case (Valid(_), Valid(_: RuntimeEvaluableTree) | _: Invalid) => cls
          case (Valid(c), Valid(ct: ClassTree)) =>
            if (c.`type`.isStatic()) cls
            else CompilerRecoverable(s"Cannot access non-static class ${c.`type`.name} from ${ct.`type`.name()}")
          case (_, Valid(value)) => findOuter(value).flatMap(o => validateClass(name, Valid(o)))
          case (_, _: Invalid) => Recoverable(s"Cannot find class $name")
        }
      }

  def validateName(
      value: String,
      of: Validation[RuntimeTree],
      methodFirst: Boolean = false
  ): Validation[RuntimeEvaluableTree] = {
    val name = NameTransformer.encode(value)
    def field = fieldTreeByName(of, name)
    def zeroArg = zeroArgMethodTreeByName(of, name)
    def member =
      if (methodFirst) zeroArg.orElse(field)
      else field.orElse(zeroArg)

    of.flatMap { of =>
      member
        .orElse(validateModule(name, Some(of)))
        .orElse(findOuter(of).flatMap(o => validateName(value, Valid(o), methodFirst)))
    }.orElse(validateModule(name, None))
      .orElse(localVarTreeByName(name))
  }

  /* -------------------------------------------------------------------------- */
  /*                              Apply validation                              */
  /* -------------------------------------------------------------------------- */
  def validateApply(
      on: RuntimeTree,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[RuntimeEvaluableTree] =
    methodTreeByNameAndArgs(on, "apply", args)
      .orElse { ArrayElemTree(on, "apply", args) }

  def validateIndirectApply(
      on: Validation[RuntimeTree],
      name: String,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[RuntimeEvaluableTree] =
    for {
      intermediate <- validateName(name, on, methodFirst = true).orElse(validateClass(name, on))
      result <- validateApply(intermediate, args)
    } yield result

  /*
   * validateIndirectApply MUST be called before methodTreeByNameAndArgs
   * That's because at runtime, a inner module is accessible as a 0-arg method,
   * and if its associated class has not attribute either, when calling the
   * constructor of the class, methodTreeByNameAndArgs would return its companion
   * object instead. Look at the test about multiple layers for an example
   */
  def findMethod(
      tree: RuntimeTree,
      name: String,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[RuntimeEvaluableTree] =
    validateIndirectApply(Valid(tree), name, args)
      .orElse { methodTreeByNameAndArgs(tree, name, args) }
      .orElse { validateApply(tree, args) }
      .orElse { findOuter(tree).flatMap(findMethod(_, name, args)) }

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
  def validateSelect(select: Term.Select): Validation[RuntimeEvaluableTree] =
    for {
      qual <- validateWithClass(select.qual)
      select <- validateName(select.name.value, Valid(qual))
    } yield select

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
    } yield NewInstanceTree(method)
  }
}

object RuntimeDefaultValidator {
  def apply(frame: JdiFrame, logger: Logger) =
    new RuntimeDefaultValidator(frame, logger)
}
