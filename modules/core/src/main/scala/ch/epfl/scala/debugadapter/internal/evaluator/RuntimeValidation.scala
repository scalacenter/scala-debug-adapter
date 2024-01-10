package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.NameTransformer
import ch.epfl.scala.debugadapter.internal.SourceLookUpProvider
import com.sun.jdi.*

import scala.jdk.CollectionConverters.*
import scala.meta.parsers.*
import scala.meta.trees.*
import scala.meta.{Type => MType, *}
import scala.util.Failure
import scala.util.Success

import scala.collection.mutable.Buffer
import scala.util.Try

case class Call(fun: Term, argClause: Term.ArgClause)
case class PreparedCall(qual: Validation[RuntimeTree], name: String)

private[evaluator] class RuntimeValidation(frame: JdiFrame, sourceLookUp: SourceLookUpProvider, preEvaluation: Boolean)(
    implicit logger: Logger
) {
  private val evaluation = new RuntimeEvaluation(frame, logger)

  def validate(expression: String): Validation[RuntimeEvaluableTree] =
    parse(expression).flatMap(validate)

  private def parse(expression: String): Validation[Stat] =
    expression.parse[Stat] match {
      case err: Parsed.Error => Fatal(err.details)
      case Parsed.Success(tree) => Valid(tree)
    }

  private def validate(expression: Stat): Validation[RuntimeEvaluableTree] =
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

  private def preEvaluate(tree: RuntimeEvaluableTree): Validation[RuntimeEvaluableTree] = {
    if (preEvaluation) {
      val value = evaluation.evaluate(tree)
      var tpe = value.extract(_.value.`type`)
      Validation.fromTry(tpe).map(RuntimeValueTree(value, _))
    } else Valid(tree)
  }

  protected def validateWithClass(expression: Stat): Validation[RuntimeTree] =
    expression match {
      case value: Term.Name =>
        validateName(value.value, false).orElse(validateClass(value.value))
      case Term.Select(qual, name) =>
        validateWithClass(qual).transform {
          case Valid(qual) =>
            validateMember(name.value, qual).orElse(validateClass(name.value, qual))
          case _: Invalid =>
            searchClassesQCN(qual.toString + "." + name.value)
        }
      case _ => validate(expression)
    }

  /* -------------------------------------------------------------------------- */
  /*                              Block validation                              */
  /* -------------------------------------------------------------------------- */
  private def validateBlock(block: Term.Block): Validation[RuntimeEvaluableTree] =
    block.stats.foldLeft(unitTree) {
      case (Valid(_), stat) => validate(stat)
      case (err: Invalid, _) => err
    }

  private def unitTree: Validation[RuntimeEvaluableTree] = validateLiteral(Lit.Unit())

  /* -------------------------------------------------------------------------- */
  /*                             Literal validation                             */
  /* -------------------------------------------------------------------------- */
  private def validateLiteral(lit: Lit): Validation[RuntimeEvaluableTree] =
    classLoader.map { loader =>
      val value = loader.mirrorOfLiteral(lit.value)
      val tpe = loader
        .mirrorOfLiteral(lit.value)
        .map(_.value.`type`)
        .extract
        .get
      RuntimeValueTree(value, tpe)
    }

  private def classLoader: Validation[JdiClassLoader] =
    Validation.fromTry(frame.classLoader().getResult)

  /* -------------------------------------------------------------------------- */
  /*                               This validation                              */
  /* -------------------------------------------------------------------------- */
  lazy val thisTree: Validation[RuntimeEvaluableTree] =
    Validation.fromOption {
      frame.thisObject
        .map(ths => ThisTree(ths.reference.referenceType().asInstanceOf[ClassType]))
    }

  lazy val declaringType: Validation[ReferenceType] =
    Validation(frame.current().location().declaringType())

  lazy val currentLocation: Validation[RuntimeTree] = thisTree.orElse {
    frame.current().location().declaringType() match {
      case ct: ClassType => Valid(ClassTree(ct))
      case _ => Recoverable("Cannot get current location")
    }
  }

  /* -------------------------------------------------------------------------- */
  /*                               Name validation                              */
  /* -------------------------------------------------------------------------- */
  def localVarTreeByName(name: String, preevaluate: Boolean = preEvaluation): Validation[RuntimeEvaluableTree] = {
    val validation = Validation
      .fromOption(frame.variableByName(name))
      .filter(_.`type`.name() != "scala.Function0", runtimeFatal = true)
      .map(v => LocalVarTree(name, v.`type`))

    validation.transform {
      case Valid(tree) if preevaluate => preEvaluate(tree)
      case tree => tree
    }
  }

  // We might sometimes need to access a 'private' attribute of a class
  private def fieldLookup(name: String, ref: ReferenceType): Option[Field] =
    Option(ref.fieldByName(name))
      .orElse(ref.visibleFields().asScala.find(_.name().endsWith("$" + name)))

  def fieldTreeByName(
      of: RuntimeTree,
      name: String,
      preevaluate: Boolean = preEvaluation
  ): Validation[RuntimeEvaluableTree] = {
    val validation = for {
      tpe <- asReference(of.`type`)
      field <- Validation.fromOption(fieldLookup(name, tpe))
      _ = loadClassOnNeed(field)
      fieldTree <- toStaticIfNeeded(field, of)
    } yield fieldTree
    validation.transform {
      case tree if !preevaluate => tree
      case Valid(tree @ (_: StaticFieldTree | InstanceFieldTree(_, _: RuntimeValueTree))) =>
        preEvaluate(tree)
      case Valid(tree @ (_: TopLevelModuleTree | NestedModuleTree(_, InstanceMethodTree(_, _, _: RuntimeValueTree)))) =>
        preEvaluate(tree)
      case tree => tree
    }
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
    val validation = searchClasses(moduleName, ofName).flatMap { moduleCls =>
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
    validation.transform {
      case Valid(tree @ (_: TopLevelModuleTree | NestedModuleTree(_, InstanceMethodTree(_, _, _: RuntimeValueTree)))) =>
        preEvaluate(tree)
      case tree => tree
    }
  }

  def validateClass(name: String): Validation[ClassTree] =
    thisTree match {
      case Valid(thisTree) => validateClass(name, thisTree)
      case _: Invalid =>
        declaringType
          .flatMap(tpe => searchClasses(name.stripSuffix("$"), Some(tpe.name)))
          .map(ClassTree.apply)
    }

  def validateClass(name: String, of: RuntimeTree): Validation[ClassTree] =
    of match {
      case of: RuntimeEvaluableTree => validateClass(name, of)
      case _: ClassTree =>
        searchClasses(name.stripSuffix("$"), Some(of.`type`.name))
          .filter(_.isStatic)
          .map(ClassTree.apply)
      case _ => Recoverable("")
    }

  def validateClass(name: String, of: RuntimeEvaluableTree): Validation[ClassTree] =
    searchClasses(name.stripSuffix("$"), Some(of.`type`.name))
      .map(ClassTree(_))
      .orElse(validateOuter(of).flatMap(o => validateClass(name, o)))

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
        .orElse(validateClass(name, on))
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
      methodTree <- UnaryOpTree(lhs, preparedCall.name)
        .orElse(BinaryOpTree(lhs, args, preparedCall.name))
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
    outerLookup(tree).transform {
      case Valid(tree @ (_: FieldTree | _: TopLevelModuleTree)) => preEvaluate(tree)
      case tree => tree
    }

  /* -------------------------------------------------------------------------- */
  /*                           Flow control validation                          */
  /* -------------------------------------------------------------------------- */
  def validateIf(tree: Term.If): Validation[RuntimeEvaluableTree] = {
    lazy val objType = loadClass("java.lang.Object").get
    val validation = for {
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
    validation.transform {
      case tree @ Valid(IfTree(RuntimeValueTree(boolean, _), thenp, elsep, _)) =>
        val predicate = for {
          boolean <- boolean
          unboxed <- boolean.unboxIfPrimitive
          bool <- unboxed.toBoolean
        } yield bool
        predicate.extract match {
          case Success(true) => Valid(thenp)
          case Success(false) => Valid(elsep)
          case _ => tree
        }
      case tree => tree
    }
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

  /* -------------------------------------------------------------------------- */
  /*                                Extractors                                  */
  /* -------------------------------------------------------------------------- */

  private object ColonEndingInfix {
    def unapply(stat: Term.ApplyInfix): Option[Term.ApplyInfix] =
      stat match {
        case Term.ApplyInfix.After_4_6_0(_, op, _, _) if (op.value.endsWith(":")) => Some(stat)
        case _ => None
      }
  }

  private object Module {
    def unapply(tpe: Type): Option[ClassType] =
      tpe match {
        case ref: ClassType if ref.fieldByName("MODULE$") != null => Some(ref)
        case _ => None
      }

    def unapply(tree: RuntimeTree): Option[RuntimeEvaluableTree] =
      tree match {
        case cls: ClassTree => unapply(cls.`type`).map(TopLevelModuleTree(_))
        case tree: RuntimeEvaluableTree => unapply(tree.`type`).map(_ => tree)
      }
  }

  private object ModuleCall {
    def unapply(m: Method): Boolean = {
      val rt = m.returnTypeName
      val noArgs = m.argumentTypeNames.size == 0
      val isSingleton = rt.endsWith("$")
      val isSingletonInstantiation = rt.stripSuffix("$").endsWith(m.name)
      noArgs && isSingleton && isSingletonInstantiation
    }
  }

  private def asReference(tpe: Type): Validation[ReferenceType] =
    tpe match {
      case tpe: ReferenceType => Valid(tpe)
      case _ => Recoverable(s"$tpe is not a reference type")
    }

  /* -------------------------------------------------------------------------- */
  /*                                Helper methods                              */
  /* -------------------------------------------------------------------------- */

  private implicit class IterableExtensions[A](iter: Iterable[A]) {
    def validateSingle(message: String): Validation[A] =
      iter.size match {
        case 1 => Valid(iter.head)
        case 0 => Recoverable(message)
        case _ => CompilerRecoverable(s"$message: multiple values found")
      }
  }

  private def fromLitToValue(literal: Lit, classLoader: JdiClassLoader): (Safe[Any], Type) = {
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

  private def moreSpecificThan(m1: Method, m2: Method): Boolean = {
    m1.argumentTypes()
      .asScala
      .zip(m2.argumentTypes().asScala)
      .forall {
        case (t1, t2) if t1.name == t2.name => true
        case (_: PrimitiveType, _) => true
        case (_, _: PrimitiveType) => true
        case (r1: ReferenceType, r2: ReferenceType) => isAssignableFrom(r1, r2)
      }
  }

  private def argsMatch(method: Method, args: Seq[Type], boxing: Boolean): Boolean =
    method.argumentTypeNames().size() == args.size && areAssignableFrom(method, args, boxing)

  /**
   * @see <a href="https://docs.oracle.com/javase/specs/jls/se20/html/jls-15.html#jls-15.12.2.5">JLS#15.12.2.5. Choosing the most specific method</a>
   *
   * @param methods the list of compatible methods to compare
   * @return a sequence containing the most precise methods
   */
  private def extractMostPreciseMethod(methods: Iterable[Method]): Seq[Method] =
    methods
      .foldLeft[List[Method]](List()) { (m1, m2) =>
        m1 match {
          case Nil => List(m2)
          case list =>
            list.flatMap { m =>
              if (moreSpecificThan(m, m2)) List(m)
              else if (moreSpecificThan(m2, m)) List(m2)
              else List(m, m2)
            }.distinct
        }
      }

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
  private def methodsByNameAndArgs(
      ref: ReferenceType,
      encodedName: String,
      args: Seq[Type]
  ): Validation[Method] = {
    val candidates = ref.methodsByName(encodedName).asScala

    val unboxedCandidates = candidates.filter(argsMatch(_, args, boxing = false))

    val boxedCandidates = unboxedCandidates.size match {
      case 0 => candidates.filter(argsMatch(_, args, boxing = true))
      case _ => unboxedCandidates
    }

    val withoutBridges = boxedCandidates.size match {
      case 0 | 1 => boxedCandidates
      case _ => boxedCandidates.filterNot(_.isBridge())
    }

    val finalCandidates = withoutBridges.size match {
      case 0 | 1 => withoutBridges
      case _ => extractMostPreciseMethod(withoutBridges)
    }

    finalCandidates
      .validateSingle(s"Cannot find a proper method $encodedName with args types $args on $ref")
      .map(loadClassOnNeed)
  }

  private def zeroArgMethodByName(ref: ReferenceType, funName: String, encode: Boolean = true): Validation[Method] = {
    val name = if (encode) NameTransformer.encode(funName) else funName

    ref.methodsByName(name).asScala.filter(_.argumentTypeNames().isEmpty()) match {
      case Buffer() => Recoverable(s"Cannot find a proper method $funName with no args on $ref")
      case Buffer(head) => Valid(head).map(loadClassOnNeed)
      case buffer =>
        buffer
          .filterNot(_.isBridge())
          .validateSingle(s"Cannot find a proper method $funName with no args on $ref")
          .map(loadClassOnNeed)
    }
  }

  private def zeroArgMethodTreeByName(
      tree: RuntimeTree,
      funName: String,
      encode: Boolean = true
  ): Validation[MethodTree] =
    asReference(tree.`type`)
      .flatMap(zeroArgMethodByName(_, funName, encode))
      .flatMap {
        case ModuleCall() =>
          Recoverable("Accessing a module from its instanciation method is not allowed at console-level")
        case mt => toStaticIfNeeded(mt, Seq.empty, tree)
      }

  private def methodTreeByNameAndArgs(
      tree: RuntimeTree,
      funName: String,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[MethodTree] =
    asReference(tree.`type`).flatMap { tpe =>
      if (!args.isEmpty) {
        methodsByNameAndArgs(tpe, NameTransformer.encode(funName), args.map(_.`type`))
          .flatMap(toStaticIfNeeded(_, args, tree))
      } else zeroArgMethodTreeByName(tree, NameTransformer.encode(funName))
    }

  private def needsOuter(cls: ClassType): Boolean =
    cls
      .methodsByName("<init>")
      .asScala
      .filter(init => init.declaringType.name == cls.name)
      .forall { init =>
        init.argumentTypeNames.asScala.headOption
          .exists { argType =>
            val suffix = argType.stripSuffix("$") + "$"
            cls.name.startsWith(suffix) && cls.name.size > suffix.size
          }
      }

  private def newInstanceTreeByArgs(cls: ClassType, args: Seq[RuntimeEvaluableTree]): Validation[NewInstanceTree] =
    methodsByNameAndArgs(cls, "<init>", args.map(_.`type`))
      .map(m => NewInstanceTree(StaticMethodTree(m, args, cls)))

  /* -------------------------------------------------------------------------- */
  /*                                Type checker                                */
  /* -------------------------------------------------------------------------- */
  private def isAssignableFrom(got: Type, expected: Type): Boolean = {
    def referenceTypesMatch(got: ReferenceType, expected: ReferenceType) = {
      val assignableFrom = expected.classObject().referenceType().methodsByName("isAssignableFrom").get(0)
      val params = Seq(got.classObject()).asJava
      expected.classObject
        .invokeMethod(frame.thread, assignableFrom, params, ObjectReference.INVOKE_SINGLE_THREADED)
        .asInstanceOf[BooleanValue]
        .value()
    }

    (got, expected) match {
      case (g: ArrayType, at: ArrayType) =>
        checkClassStatus(at.componentType())
        g.componentType().equals(at.componentType())
      case (g: PrimitiveType, pt: PrimitiveType) => got.equals(pt)
      case (g: ReferenceType, ref: ReferenceType) => referenceTypesMatch(g, ref)
      case (_: VoidType, _: VoidType) => true

      case (g: ReferenceType, pt: PrimitiveType) =>
        isAssignableFrom(g, frame.getPrimitiveBoxedClass(pt))
      case (g: PrimitiveType, ct: ReferenceType) =>
        isAssignableFrom(frame.getPrimitiveBoxedClass(g), ct)

      case _ => false
    }
  }

  private def areAssignableFrom(method: Method, args: Seq[Type], boxing: Boolean): Boolean =
    if (method.argumentTypes().size() != args.size) false
    else
      method
        .argumentTypes()
        .asScala
        .zip(args)
        .forall {
          case (_: PrimitiveType, _: ReferenceType) | (_: ReferenceType, _: PrimitiveType) if !boxing => false
          case (expected, got) => isAssignableFrom(got, expected)
        }

  /* -------------------------------------------------------------------------- */
  /*                                Class helpers                               */
  /* -------------------------------------------------------------------------- */
  private def loadClass(name: String): Validation[ClassType] =
    Validation.fromTry {
      frame
        .classLoader()
        .flatMap(_.loadClass(name))
        .extract(_.cls)
    }

  private def checkClassStatus(tpe: => Type) = Try(tpe) match {
    case Failure(e: ClassNotLoadedException) => loadClass(e.className())
    case Success(value: ClassType) if !value.isPrepared => loadClass(value.name)
    case result => result
  }

  private def loadClassOnNeed[T <: TypeComponent](tc: T): T = {
    def tpe = tc match {
      case field: Field => field.`type`
      case method: Method => method.returnType
    }
    val name = tc match {
      case field: Field => field.typeName()
      case method: Method => method.returnTypeName()
    }
    checkClassStatus(tpe)
    tc
  }

  // ! TO REFACTOR :sob:
  private def resolveInnerType(qual: Type, name: String) = {
    var tpe: Validation[ClassType] = Recoverable(s"Cannot find outer class for $qual")
    def loop(on: Type): Validation[ClassType] =
      on match {
        case _: ArrayType | _: PrimitiveType | _: VoidType =>
          Recoverable("Cannot find outer class on non reference type")
        case ref: ReferenceType =>
          val loadedCls = loadClass(concatenateInnerTypes(ref.name, name))
          if (loadedCls.isValid) loadedCls
          else {
            var superTypes: List[ReferenceType] = ref match {
              case cls: ClassType => cls.superclass() :: cls.interfaces().asScala.toList
              case itf: InterfaceType => itf.superinterfaces().asScala.toList
            }

            while (!superTypes.isEmpty && !tpe.isValid) {
              val res = loop(superTypes.head)
              if (res.isValid) tpe = res
              else superTypes = superTypes.tail
            }
            tpe
          }
      }

    loop(qual)
  }

  private def extractCommonSuperClass(tpe1: Type, tpe2: Type): Option[Type] = {
    def getSuperClasses(of: Type): Array[ClassType] =
      of match {
        case cls: ClassType =>
          Iterator.iterate(cls)(cls => cls.superclass()).takeWhile(_ != null).toArray
        case _ => Array()
      }

    val superClasses1 = getSuperClasses(tpe1)
    val superClasses2 = getSuperClasses(tpe2)
    superClasses1.find(superClasses2.contains)
  }

  private def validateType(tpe: MType, thisTree: Option[RuntimeEvaluableTree])(
      termValidation: Term => Validation[RuntimeEvaluableTree]
  ): Validation[(Option[RuntimeEvaluableTree], ClassType)] =
    tpe match {
      case MType.Name(name) =>
        // won't work if the class is defined in one of the outer of this
        searchClasses(name, thisTree.map(_.`type`.name)).map(cls => (thisTree, cls))
      case MType.Select(qual, name) =>
        val cls = for {
          qual <- termValidation(qual)
          tpe <- resolveInnerType(qual.`type`, name.value)
        } yield
          if (tpe.isStatic()) (None, tpe)
          else (Some(qual), tpe)
        cls.orElse {
          searchClassesQCN(qual.toString + "." + name.value).map(c => (None, c.`type`.asInstanceOf[ClassType]))
        }
      case _ => Recoverable("Type not supported at runtime")
    }

  // ! May not be correct when dealing with an object inside a class
  private def outerLookup(tree: RuntimeTree): Validation[RuntimeEvaluableTree] =
    asReference(tree.`type`)
      .flatMap(tpe => Validation(tpe.fieldByName("$outer")))
      .flatMap(toStaticIfNeeded(_, tree))
      .orElse {
        removeLastInnerTypeFromFQCN(tree.`type`.name())
          .map(name => loadClass(name + "$")) match {
          case Some(Valid(Module(mod))) => Valid(TopLevelModuleTree(mod))
          case res => Recoverable(s"Cannot find $$outer for ${tree.`type`.name()}}")
        }
      }

  private def searchClasses(name: String, in: Option[String]): Validation[ClassType] = {
    def baseName = in.getOrElse(frame.current().location().declaringType().name())
    val candidates = sourceLookUp.classesByName(name)
    val bestMatch = candidates.size match {
      case 0 | 1 => candidates
      case _ =>
        candidates.filter { name =>
          name.contains(s".$baseName") || name.startsWith(baseName)
        }
    }
    bestMatch
      .validateSingle(s"Cannot find class $name")
      .flatMap(loadClass)
  }

  private def searchClassesQCN(partialClassName: String): Validation[RuntimeTree] = {
    val name = NameTransformer.scalaClassName(partialClassName)
    searchClasses(name + "$", Some(partialClassName))
      .map(TopLevelModuleTree(_))
      .orElse(searchClasses(name, Some(partialClassName)).map(ClassTree(_)))
  }

  /* -------------------------------------------------------------------------- */
  /*                              Initialize module                             */
  /* -------------------------------------------------------------------------- */
  private def moduleInitializer(modCls: ClassType, of: RuntimeEvaluableTree): Validation[NestedModuleTree] =
    of.`type` match {
      case ref: ReferenceType =>
        zeroArgMethodByName(ref, NameTransformer.scalaClassName(modCls.name).stripSuffix("$"))
          .map(m => NestedModuleTree(modCls, InstanceMethodTree(m, Seq.empty, of)))
      case _ => Recoverable(s"Cannot find module initializer for non-reference type $modCls")
    }

  private def illegalAccess(x: Any, typeName: String) = Fatal {
    new ClassCastException(s"Cannot cast $x to $typeName")
  }

  /* -------------------------------------------------------------------------- */
  /*                               Useful patterns                              */
  /* -------------------------------------------------------------------------- */
  /* Standardize method calls */
  private def extractCall(apply: Stat): Call =
    apply match {
      case apply: Term.Apply => Call(apply.fun, apply.argClause)
      case ColonEndingInfix(apply) => Call(Term.Select(apply.argClause.head, apply.op), List(apply.lhs))
      case apply: Term.ApplyInfix => Call(Term.Select(apply.lhs, apply.op), apply.argClause)
      case apply: Term.ApplyUnary => Call(Term.Select(apply.arg, Term.Name("unary_" + apply.op)), List.empty)
    }

  /* -------------------------------------------------------------------------- */
  /*                           Nested types regex                          */
  /* -------------------------------------------------------------------------- */
  private def removeLastInnerTypeFromFQCN(className: String): Option[String] = {
    val (packageName, clsName) = className.splitAt(className.lastIndexOf('.') + 1)
    val name = NameTransformer.decode(clsName)
    val lastDollar = name.stripSuffix("$").lastIndexOf('$')
    lastDollar match {
      case -1 => None
      case _ => Some(packageName + name.dropRight(name.length - lastDollar))
    }
  }

  private def concatenateInnerTypes(className: String, innerName: String): String =
    if (className.endsWith("$")) className + innerName
    else className + "$" + innerName

  /* -------------------------------------------------------------------------- */
  /*                  Transformation to static or instance tree                 */
  /* -------------------------------------------------------------------------- */
  private def toStaticIfNeeded(field: Field, on: RuntimeTree): Validation[RuntimeEvaluableTree] =
    (field.`type`, on) match {
      case (Module(module), _) => Valid(TopLevelModuleTree(module))
      case (_, cls: ClassTree) =>
        if (field.isStatic) Valid(StaticFieldTree(field, cls.`type`))
        else Fatal(s"Accessing instance field $field from static context ${cls.`type`} is not allowed")
      case (_, Module(mod)) => Valid(InstanceFieldTree(field, mod))
      case (_, eval: RuntimeEvaluableTree) =>
        if (field.isStatic())
          Fatal(s"Accessing static field $field from instance ${eval.`type`} can lead to unexpected behavior")
        else Valid(InstanceFieldTree(field, eval))
    }

  private def toStaticIfNeeded(
      method: Method,
      args: Seq[RuntimeEvaluableTree],
      on: RuntimeTree
  ): Validation[MethodTree] = on match {
    case cls: ClassTree =>
      if (method.isStatic()) Valid(StaticMethodTree(method, args, cls.`type`))
      else Fatal(s"Accessing instance method $method from static context ${cls.`type`} is not allowed")
    case Module(mod) => Valid(InstanceMethodTree(method, args, mod))
    case eval: RuntimeEvaluableTree =>
      if (method.isStatic())
        Fatal(s"Accessing static method $method from instance ${eval.`type`} can lead to unexpected behavior")
      else Valid(InstanceMethodTree(method, args, eval))
  }
}
