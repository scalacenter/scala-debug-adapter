package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.NameTransformer
import ch.epfl.scala.debugadapter.internal.SourceLookUpProvider
import com.sun.jdi.*

import scala.jdk.CollectionConverters.*
import scala.meta.parsers.*
import scala.meta.trees.*
import scala.meta.{Type => _, *}
import scala.util.Failure
import scala.util.Success

import RuntimeEvaluatorExtractors.*

case class Call(fun: Term, argClause: Term.ArgClause)
case class PreparedCall(qual: Validation[RuntimeTree], name: String)

class RuntimeDefaultValidator(val frame: JdiFrame, val sourceLookUp: SourceLookUpProvider, implicit val logger: Logger)
    extends RuntimeValidator {
  val helper = new RuntimeEvaluationHelpers(frame, sourceLookUp)
  import helper.*

  protected def parse(expression: String): Validation[Stat] =
    expression.parse[Stat] match {
      case err: Parsed.Error => Fatal(err.details)
      case Parsed.Success(tree) => Valid(tree)
    }

  def validate(expression: String): Validation[RuntimeEvaluableTree] =
    parse(expression).flatMap(validate)

  def validate(expression: Stat): Validation[RuntimeEvaluableTree] =
    expression match {
      case lit: Lit => validateLiteral(lit)
      case value: Term.Name => validateName(value.value, false)
      case _: Term.This => thisTree
      case sup: Term.Super => Recoverable("Super not (yet) supported at runtime")
      case _: Term.Apply | _: Term.ApplyInfix | _: Term.ApplyUnary => validateMethod(extractCall(expression))
      case select: Term.Select => validateSelect(select)
      case branch: Term.If => validateIf(branch)
      case instance: Term.New => validateNew(instance)
      case block: Term.Block => validateBlock(block)
      case assign: Term.Assign => validateAssign(assign)
      case _ => Recoverable("Expression not supported at runtime")
    }

  protected def validateWithClass(expression: Stat): Validation[RuntimeTree] =
    expression match {
      case value: Term.Name => validateName(value.value, false).orElse(validateClass(value.value, currentLocation))
      case Term.Select(qual, name) =>
        validateWithClass(qual).transform {
          case qual @ Valid(q) =>
            validateMember(name.value, q)
              .orElse(validateClass(name.value, qual))
          case _: Invalid =>
            searchClassesQCN(qual.toString + "." + name.value)
        }
      case _ => validate(expression)
    }

  /* -------------------------------------------------------------------------- */
  /*                              Block validation                              */
  /* -------------------------------------------------------------------------- */
  def validateBlock(block: Term.Block): Validation[RuntimeEvaluableTree] =
    block.stats.foldLeft(Valid(UnitTree): Validation[RuntimeEvaluableTree]) {
      case (Valid(_), stat) => validate(stat)
      case (err: Invalid, _) => err
    }

  /* -------------------------------------------------------------------------- */
  /*                             Literal validation                             */
  /* -------------------------------------------------------------------------- */
  def validateLiteral(lit: Lit): Validation[RuntimeEvaluableTree] =
    frame.classLoader().map(loader => LiteralTree(fromLitToValue(lit, loader))).extract match {
      case Success(value) => value
      case Failure(e) => CompilerRecoverable(e)
    }

  /* -------------------------------------------------------------------------- */
  /*                               This validation                              */
  /* -------------------------------------------------------------------------- */
  lazy val thisTree: Validation[RuntimeEvaluableTree] =
    Validation.fromOption {
      frame.thisObject
        .map(ths => ThisTree(ths.reference.referenceType().asInstanceOf[ClassType]))
    }
  lazy val currentLocation: Validation[RuntimeTree] = thisTree.orElse {
    frame.current().location().declaringType() match {
      case ct: ClassType => Valid(StaticTree(ct))
      case _ => Recoverable("Cannot get current location")
    }
  }

  /* -------------------------------------------------------------------------- */
  /*                               Name validation                              */
  /* -------------------------------------------------------------------------- */
  def localVarTreeByName(name: String, preevaluate: Boolean = true): Validation[RuntimeEvaluableTree] =
    Validation
      .fromOption(frame.variableByName(name))
      .filter(_.`type`.name() != "scala.Function0", runtimeFatal = true)
      .map(v => LocalVarTree(name, v.`type`))

  // We might sometimes need to access a 'private' attribute of a class
  private def fieldLookup(name: String, ref: ReferenceType) =
    Option(ref.fieldByName(name))
      .orElse(ref.visibleFields().asScala.find(_.name().endsWith("$" + name)))

  def fieldTreeByName(
      of: RuntimeTree,
      name: String,
      preevaluate: Boolean = true
  ): Validation[RuntimeEvaluableTree] =
    of match {
      case ReferenceTree(ref) =>
        for {
          field <- Validation.fromOption(fieldLookup(name, ref))
          _ = loadClassOnNeed(field)
          fieldTree <- toStaticIfNeeded(field, of)
        } yield fieldTree
      case _ => Recoverable(s"Cannot access field $name from non reference type ${of.`type`.name()}")
    }

  private def inCompanion(name: Option[String], moduleName: String) = name
    .filter(_.endsWith("$"))
    .map(n => loadClass(n.stripSuffix("$")))
    .exists {
      _.filterNot {
        _.methodsByName(moduleName.stripSuffix("$")).isEmpty()
      }.isValid
    }

  def validateModule(name: String, of: Option[RuntimeTree]): Validation[RuntimeEvaluableTree] = {
    val moduleName = if (name.endsWith("$")) name else name + "$"
    val ofName = of.map(_.`type`.name)
    searchClasses(moduleName, ofName).flatMap { moduleCls =>
      val isInModule = inCompanion(ofName, moduleName)

      (isInModule, moduleCls, of) match {
        case (true, _, _) => CompilerRecoverable(s"Cannot access module $name from $ofName")
        case (_, Module(_), _) => Valid(TopLevelModuleTree(moduleCls))
        case (_, cls, Some(instance: RuntimeEvaluableTree)) =>
          if (cls.name.startsWith(instance.`type`.name()))
            moduleInitializer(cls, instance)
          else Recoverable(s"Cannot access module $moduleCls from ${instance.`type`.name()}")
        case _ => Recoverable(s"Cannot access module $moduleCls")
      }
    }
  }

  def validateClass(name: String, of: Validation[RuntimeTree]): Validation[ClassTree] =
    searchClasses(name.stripSuffix("$"), of.map(_.`type`.name()).toOption)
      .flatMap { cls =>
        of match {
          case Valid(_: RuntimeEvaluableTree | _: StaticTree) | _: Invalid => Valid(ClassTree(cls))
          case Valid(ct: ClassTree) =>
            if (cls.isStatic()) Valid(ClassTree(cls))
            else CompilerRecoverable(s"Cannot access non-static class ${cls.name} from ${ct.`type`.name()}")
        }
      }
      .orElse {
        of match {
          case Valid(value) => validateOuter(value).flatMap(o => validateClass(name, Valid(o)))
          case _ => Recoverable(s"Cannot access class $name")
        }
      }

  def validateMember(
      name: String,
      of: RuntimeTree,
      methodFirst: Boolean = false
  ): Validation[RuntimeEvaluableTree] = {
    val encodedName = NameTransformer.encode(name)
    def field = fieldTreeByName(of, encodedName)
    def zeroArg = zeroArgMethodTreeByName(of, encodedName)
    def member =
      if (methodFirst) zeroArg.orElse(field)
      else field.orElse(zeroArg)

    member
      .orElse(validateModule(name, Some(of)))
      .orElse(validateOuter(of).flatMap(validateMember(name, _, methodFirst)))
  }

  def validateName(name: String, methodFirst: Boolean): Validation[RuntimeEvaluableTree] =
    localVarTreeByName(NameTransformer.encode(name))
      .orElse(currentLocation.flatMap(validateMember(name, _, methodFirst)))

  /* -------------------------------------------------------------------------- */
  /*                              Apply validation                              */
  /* -------------------------------------------------------------------------- */
  def validateApply(
      on: RuntimeTree,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[RuntimeEvaluableTree] =
    methodTreeByNameAndArgs(on, "apply", args)
      .orElse(ArrayElemTree(on, args))

  def validateIndirectApply(
      on: RuntimeTree,
      name: String,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[RuntimeEvaluableTree] =
    for {
      intermediate <- validateMember(name, on, methodFirst = true)
        .orElse(localVarTreeByName(name))
        .orElse(validateClass(name, Valid(on)))
      result <- validateApply(intermediate, args)
    } yield result

  def findMethod(
      tree: RuntimeTree,
      name: String,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[RuntimeEvaluableTree] =
    methodTreeByNameAndArgs(tree, name, args)
      .orElse(validateIndirectApply(tree, name, args))
      .orElse(validateApply(tree, args))
      .orElse(validateOuter(tree).flatMap(findMethod(_, name, args)))

  def validateMethod(call: Call): Validation[RuntimeEvaluableTree] = {
    lazy val preparedCall = call.fun match {
      case Term.Select(qual, name) =>
        val qualTree = validateWithClass(qual).orElse {
          searchClassesQCN(qual.toString + "$")
        }
        PreparedCall(qualTree, name.value)
      case name: Term.Name => PreparedCall(currentLocation, name.value)
    }

    val validatedArgs = call.argClause.map(validate).traverse
    val method = for {
      args <- validatedArgs
      lhs <- preparedCall.qual
      methodTree <- PrimitiveUnaryOpTree(lhs, preparedCall.name)
        .orElse(PrimitiveBinaryOpTree(lhs, args, preparedCall.name))
        .orElse(findMethod(lhs, preparedCall.name, args))
    } yield methodTree

    call.fun match {
      case Term.Select(qual, name) =>
        method.orElse {
          for {
            cls <- searchClassesQCN(qual.toString + "." + name.value)
            args <- validatedArgs
            m <- validateApply(cls, args)
          } yield m
        }
      case _ => method
    }
  }

  /* -------------------------------------------------------------------------- */
  /*                              Select validation                             */
  /* -------------------------------------------------------------------------- */
  def validateSelect(select: Term.Select): Validation[RuntimeEvaluableTree] =
    for {
      qual <- validateWithClass(select.qual)
      select <- validateMember(select.name.value, qual)
    } yield select

  /* -------------------------------------------------------------------------- */
  /*                               New validation                               */
  /* -------------------------------------------------------------------------- */
  def validateNew(newValue: Term.New): Validation[RuntimeEvaluableTree] = {
    val tpe = newValue.init.tpe
    val argClauses = newValue.init.argClauses
    for {
      args <- argClauses.flatMap(_.map(validate(_))).traverse
      (outer, cls) <- validateType(tpe, thisTree.toOption)(validate)
      allArgs = outer.filter(_ => needsOuter(cls)).toSeq ++ args
      newInstance <- newInstanceTreeByArgs(cls, allArgs)
    } yield newInstance
  }

  /* -------------------------------------------------------------------------- */
  /*                              Outer validation                              */
  /* -------------------------------------------------------------------------- */
  def validateOuter(tree: RuntimeTree): Validation[RuntimeEvaluableTree] =
    outerLookup(tree)

  /* -------------------------------------------------------------------------- */
  /*                           Flow control validation                          */
  /* -------------------------------------------------------------------------- */
  def validateIf(tree: Term.If): Validation[RuntimeEvaluableTree] = {
    lazy val objType = loadClass("java.lang.Object").get
    for {
      cond <- validate(tree.cond)
      thenp <- validate(tree.thenp)
      elsep <- validate(tree.elsep)
      ifTree <- IfTree(
        cond,
        thenp,
        elsep,
        isAssignableFrom(_, _),
        extractCommonSuperClass(thenp.`type`, elsep.`type`).getOrElse(objType)
      )
    } yield ifTree
  }

  /* -------------------------------------------------------------------------- */
  /*                              Assign validation                             */
  /* -------------------------------------------------------------------------- */
  def isMutable(tree: RuntimeEvaluableTree): Boolean =
    tree match {
      case field: FieldTree => !field.immutable
      case localVar: LocalVarTree => true
      case _ => false
    }

  def validateAssign(tree: Term.Assign): Validation[RuntimeEvaluableTree] = {
    val lhs = tree.lhs match {
      case select: Term.Select =>
        validateWithClass(select.qual).flatMap(fieldTreeByName(_, select.name.value, false))
      case name: Term.Name =>
        localVarTreeByName(name.value, false).orElse(currentLocation.flatMap(fieldTreeByName(_, name.value, false)))
      case _ => Recoverable("Unsupported assignment")
    }

    for {
      lhsValue <- lhs
      if isMutable(lhsValue)
      rhs <- validate(tree.rhs)
      if isAssignableFrom(rhs.`type`, lhsValue.`type`)
      unit = frame.thread.virtualMachine().mirrorOfVoid().`type`()
      assign <- AssignTree(lhsValue, rhs, unit)
    } yield assign
  }
}

object RuntimeDefaultValidator {
  def apply(frame: JdiFrame, sourceLookUp: SourceLookUpProvider, logger: Logger) =
    new RuntimeDefaultValidator(frame, sourceLookUp, logger)
}
