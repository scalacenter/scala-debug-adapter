package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.NameTransformer
import ch.epfl.scala.debugadapter.internal.SourceLookUpProvider
import ch.epfl.scala.debugadapter.internal.evaluator.RuntimeEvaluationTree.*
import com.sun.jdi

import scala.jdk.CollectionConverters.*
import scala.meta.parsers.*
import scala.meta.trees.*
import scala.meta.{Type => _, *}
import scala.util.Failure
import scala.util.Success

import scala.collection.mutable.Buffer
import scala.util.Try

private[evaluator] class RuntimeValidation(frame: JdiFrame, sourceLookUp: SourceLookUpProvider, preEvaluation: Boolean)(
    implicit logger: Logger
) {
  private val evaluation = new RuntimeEvaluation(frame, logger)
  private def classLoader: Validation[JdiClassLoader] =
    Validation.fromTry(frame.classLoader().getResult)

  def validate(expression: String): Validation[RuntimeEvaluationTree] =
    parse(expression).flatMap(validateAsValue)

  private def parse(expression: String): Validation[Stat] =
    expression.parse[Stat] match {
      case err: Parsed.Error => Fatal(err.details)
      case Parsed.Success(tree) => Valid(tree)
    }

  private def validateAsValue(expression: Stat): Validation[RuntimeEvaluationTree] =
    expression match {
      case lit: Lit => validateLiteral(lit)
      case Term.Name(name) =>
        findVariable(name)
          .orElse(thisTree.flatMap(findMemberOrModule(name, _)))
          .orElse(declaringType.flatMap(findStaticMember(name, _)))
          .orElse(findTopLevelModule(name))
      case _: Term.This => thisTree
      case sup: Term.Super => Recoverable("Super not (yet) supported at runtime")
      case _: Term.Apply | _: Term.ApplyInfix | _: Term.ApplyUnary => validateMethod(standardize(expression))
      case select: Term.Select =>
        validateAsValue(select.qual)
          .flatMap(findMemberOrModule(select.name.value, _))
          .orElse(validateAsClass(select.qual).flatMap(findStaticMember(select.name.value, _)))
          .orElse(findTopLevelModule(select.qual.toString, select.name.value))
      case branch: Term.If => validateIf(branch)
      case instance: Term.New => validateNew(instance)
      case block: Term.Block => validateBlock(block)
      case assign: Term.Assign => validateAssign(assign)
      case _ => Recoverable("Expression not supported at runtime")
    }

  private lazy val thisTree: Validation[RuntimeEvaluationTree] =
    Validation.fromOption(frame.thisObject.map(ths => This(ths.reference.referenceType())))

  private lazy val declaringType: Validation[jdi.ReferenceType] =
    Validation(frame.current().location().declaringType())

  private lazy val currentPackage: Validation[String] =
    declaringType.map(_.name.reverse.dropWhile(_ != '.').reverse)

  private def validateAsClass(expression: Stat): Validation[jdi.ReferenceType] =
    expression match {
      case Term.Name(name) => findClass(name)
      case Term.Select(qualifier, Name(name)) =>
        validateAsValue(qualifier)
          .flatMap(findClassMember(name, _))
          .orElse(validateAsClass(qualifier).flatMap(findStaticClass(name, _)))
          .orElse(findQualifiedClass(name, qualifier.toString))
      case _ => Recoverable("not a class")
    }

  private def preEvaluate(tree: RuntimeEvaluationTree): Validation[RuntimeEvaluationTree] = {
    def eval = {
      val value = evaluation.evaluate(tree)
      var tpe = value.extract(_.value.`type`)
      Validation.fromTry(tpe).map(Value(value, _)).orElse(Valid(tree))
    }

    if (preEvaluation) {
      tree match {
        case CallBinaryOp(_: Value, _: Value, _) => eval
        case ArrayElem(_: Value, _: Value, _) => eval
        case If(Value(cond, _), thenp, elsep, _) =>
          val boolean = for {
            cond <- cond
            unboxed <- cond.unboxIfPrimitive
            bool <- unboxed.toBoolean
          } yield bool
          boolean.extract match {
            case Success(true) => Valid(thenp)
            case Success(false) => Valid(elsep)
            case _ => Valid(tree)
          }
        case InstanceField(_, _: Value) => eval
        case _: StaticField => eval
        case LocalVar(_, _) => eval
        case CallUnaryOp(_: Value, _) => eval
        case NestedModule(_, CallInstanceMethod(_, _, _: Value)) => eval
        case _: StaticModule => eval
        case _: This => eval
        case _ => Valid(tree)
      }
    } else Valid(tree)
  }

  private def validateBlock(block: Term.Block): Validation[RuntimeEvaluationTree] =
    block.stats.foldLeft(unitTree) {
      case (Valid(_), stat) => validateAsValue(stat)
      case (err: Invalid, _) => err
    }

  private def unitTree: Validation[RuntimeEvaluationTree] = validateLiteral(Lit.Unit())

  private def validateLiteral(lit: Lit): Validation[RuntimeEvaluationTree] =
    classLoader.map { loader =>
      val value = loader.mirrorOfLiteral(lit.value)
      val tpe = loader
        .mirrorOfLiteral(lit.value)
        .map(_.value.`type`)
        .extract
        .get
      Value(value, tpe)
    }

  private def findVariable(name: String, preevaluate: Boolean = preEvaluation): Validation[RuntimeEvaluationTree] = {
    val encodedName = NameTransformer.encode(name)
    Validation
      .fromOption(frame.variableByName(encodedName))
      .filter(_.`type`.name != "scala.Function0")
      .map(v => LocalVar(encodedName, v.`type`))
      .flatMap(v => if (preevaluate) preEvaluate(v) else Valid(v))
  }

  private def findField(name: String, ref: jdi.ReferenceType): Option[jdi.Field] = {
    val encodedName = NameTransformer.encode(name)
    Option(ref.fieldByName(encodedName))
      .orElse(ref.visibleFields.asScala.find(_.name.endsWith("$" + encodedName)))
  }

  private def findField(
      qualifier: RuntimeEvaluationTree,
      name: String,
      preevaluate: Boolean = preEvaluation
  ): Validation[RuntimeEvaluationTree] = {
    for {
      qualifierTpe <- asReference(qualifier.`type`)
      field <- Validation.fromOption(findField(name, qualifierTpe))
      _ = loadClassOnNeed(field)
      fieldTree <- asInstanceField(field, qualifier, preevaluate)
    } yield fieldTree
  }

  private def findStaticField(
      qualifier: jdi.ReferenceType,
      name: String,
      preevaluate: Boolean = preEvaluation
  ): Validation[RuntimeEvaluationTree] = {
    for {
      field <- Validation.fromOption(findField(name, qualifier))
      _ = loadClassOnNeed(field)
      fieldTree <- asStaticField(field, preevaluate = preevaluate)
    } yield fieldTree
  }

  private def findClass(name: String): Validation[jdi.ReferenceType] =
    thisTree
      .flatMap(findClassMember(name, _))
      .orElse(declaringType.flatMap(findStaticClass(name, _)))
      .orElse(findTopLevelClass(name))

  private def findStaticClass(name: String, qualifier: jdi.ReferenceType): Validation[jdi.ReferenceType] =
    findQualifiedClass(name, qualifier.name).filter(_.isStatic)

  private def findClassMember(name: String, qualifier: RuntimeEvaluationTree): Validation[jdi.ReferenceType] =
    findQualifiedClass(name, qualifier.`type`.name)
      .orElse(findOuter(qualifier).flatMap(outer => findClassMember(name, outer)))

  private def findQualifiedClass(name: String, qualifier: String): Validation[jdi.ClassType] = {
    val encodedQualifier = qualifier.split('.').map(NameTransformer.encode).mkString(".")
    findAllFullyQualifiedClassNames(name)
      .filter(fqcn => fqcn.contains(encodedQualifier))
      .validateSingle(s"Cannot find class $name in qualifier $qualifier")
      .flatMap(loadClass)
  }

  private def findTopLevelClass(name: String): Validation[jdi.ClassType] = {
    val currentPackage = declaringType.map(_.name.reverse.dropWhile(_ != '.').reverse).toOption
    val fqcns = findAllFullyQualifiedClassNames(name)
    val candidates = currentPackage match {
      case Some(currentPackage) if fqcns.size > 1 =>
        // if there is ambiguity we prefer the class in the current package
        val candidates = fqcns.filter(_.startsWith(currentPackage))
        if (candidates.size > 0) candidates else fqcns
      case _ => fqcns
    }
    candidates.validateSingle(s"Cannot find top level class $name").flatMap(loadClass)
  }

  private def findAllFullyQualifiedClassNames(name: String): Seq[String] = {
    val scalaClassName = NameTransformer.scalaClassName(name)
    val encodedName = NameTransformer.encode(name)
    sourceLookUp.classesByScalaName(scalaClassName).filter(fqcn => fqcn.endsWith(encodedName))
  }

  private def findMemberOrModule(name: String, qualifier: RuntimeEvaluationTree): Validation[RuntimeEvaluationTree] =
    findField(qualifier, name)
      .orElse(findZeroArgMethod(qualifier, name))
      .orElse(findModule(name, qualifier))
      .orElse(findOuter(qualifier).flatMap(findMemberOrModule(name, _)))

  private def findModule(name: String, qualifier: RuntimeEvaluationTree): Validation[RuntimeEvaluationTree] = {
    val moduleClassName = name.stripSuffix("$") + "$"
    val qualifierTypeName = qualifier.`type`.name
    findClassMember(moduleClassName, qualifier).flatMap { tpe =>
      if (inCompanion(Some(qualifierTypeName), moduleClassName))
        CompilerRecoverable(s"Cannot access module $name from $qualifierTypeName")
      else asModule(tpe, qualifier)
    }
  }

  private def inCompanion(name: Option[String], moduleName: String) =
    name
      .filter(_.endsWith("$"))
      .map(n => loadClass(n.stripSuffix("$")))
      .exists(_.filterNot(_.methodsByName(moduleName.stripSuffix("$")).isEmpty()).isValid)

  private def findTopLevelModule(name: String): Validation[RuntimeEvaluationTree] =
    findTopLevelClass(name.stripSuffix("$") + "$").flatMap(asStaticModule)

  private def findTopLevelModule(pkg: String, name: String): Validation[RuntimeEvaluationTree] =
    findQualifiedClass(name.stripSuffix("$") + "$", pkg).flatMap(asStaticModule)

  private def findStaticMember(name: String, qualifier: jdi.ReferenceType): Validation[RuntimeEvaluationTree] =
    findStaticField(qualifier, name).orElse(findZeroArgStaticMethod(qualifier, name))

  /* Standardized method call */
  private case class Call(fun: Term, argClause: Term.ArgClause)

  private def standardize(apply: Stat): Call =
    apply match {
      case apply: Term.Apply => Call(apply.fun, apply.argClause)
      case Term.ApplyInfix.After_4_6_0(lhs, op, _, argClause) if op.value.endsWith(":") =>
        Call(Term.Select(argClause.head, op), List(lhs))
      case apply: Term.ApplyInfix => Call(Term.Select(apply.lhs, apply.op), apply.argClause)
      case apply: Term.ApplyUnary => Call(Term.Select(apply.arg, Term.Name("unary_" + apply.op)), List.empty)
    }

  private def validateMethod(call: Call): Validation[RuntimeEvaluationTree] = {
    call.argClause.map(validateAsValue).traverse.flatMap { args =>
      call.fun match {
        case Term.Select(qualifier, Name(name)) =>
          validateAsValue(qualifier)
            .flatMap { qualifier =>
              CallUnaryOp(qualifier, name)
                .orElse(CallBinaryOp(qualifier, args, name))
                .orElse(findMethodBySignedName(qualifier, name, args))
            }
            .orElse(validateAsValue(call.fun).flatMap(findApplyMethod(_, args)))
            .orElse(validateAsClass(qualifier).flatMap(findStaticMethodBySignedName(_, name, args)))
        case Term.Name(name) =>
          thisTree
            .flatMap(findMethodInThisOrOuter(_, name, args))
            .orElse(validateAsValue(call.fun).flatMap(findApplyMethod(_, args)))
            .orElse(declaringType.flatMap(findStaticMethodBySignedName(_, name, args)))
      }
    }
  }

  private def findApplyMethod(
      qualifier: RuntimeEvaluationTree,
      args: Seq[RuntimeEvaluationTree]
  ): Validation[RuntimeEvaluationTree] =
    findMethodBySignedName(qualifier, "apply", args).orElse(ArrayElem(qualifier, args))

  private def findMethodInThisOrOuter(
      thisOrOuter: RuntimeEvaluationTree,
      name: String,
      args: Seq[RuntimeEvaluationTree]
  ): Validation[RuntimeEvaluationTree] =
    CallUnaryOp(thisOrOuter, name)
      .orElse(CallBinaryOp(thisOrOuter, args, name))
      .orElse(findMethodBySignedName(thisOrOuter, name, args))
      .orElse(findOuter(thisOrOuter).flatMap(findMethodInThisOrOuter(_, name, args)))

  def validateNew(newValue: Term.New): Validation[RuntimeEvaluationTree] = {
    val tpe = newValue.init.tpe
    val argClauses = newValue.init.argClauses
    for {
      args <- argClauses.flatMap(_.map(validateAsValue)).traverse
      (outer, cls) <- validateType(tpe, thisTree.toOption)
      allArgs = outer.filter(_ => needsOuter(cls)).toSeq ++ args
      newInstance <- newInstanceTreeByArgs(cls, allArgs)
    } yield newInstance
  }

  // ! May not be correct when dealing with an object inside a class
  private def findOuter(qualifier: RuntimeEvaluationTree): Validation[RuntimeEvaluationTree] =
    asReference(qualifier.`type`)
      .flatMap(tpe => Validation(tpe.fieldByName("$outer")))
      .flatMap(asInstanceField(_, qualifier, preevaluate = true))
      .orElse {
        Validation
          .fromOption(removeLastInnerTypeFromFQCN(qualifier.`type`.name))
          .flatMap(name => loadClass(name + "$"))
          .flatMap(asStaticModule)
      }

  private def validateIf(tree: Term.If): Validation[RuntimeEvaluationTree] =
    for {
      cond <- validateAsValue(tree.cond)
      thenp <- validateAsValue(tree.thenp)
      elsep <- validateAsValue(tree.elsep)
      ifTree <- asIfTree(cond, thenp, elsep)
    } yield ifTree

  private def asIfTree(
      cond: RuntimeEvaluationTree,
      ifTrue: RuntimeEvaluationTree,
      ifFalse: RuntimeEvaluationTree
  ): Validation[RuntimeEvaluationTree] = {
    val tType = ifTrue.`type`
    val fType = ifFalse.`type`

    if (isBoolean(cond.`type`)) {
      val tpe =
        if (isAssignableFrom(tType, fType)) Valid(tType)
        else if (isAssignableFrom(fType, tType)) Valid(fType)
        else Validation.fromOption(getCommonSuperClass(tType, fType)).orElse(loadClass("java.lang.Object"))
      tpe.flatMap(tpe => preEvaluate(If(cond, ifTrue, ifFalse, tpe)))
    } else CompilerRecoverable("A predicate must be a boolean")
  }

  private def isBoolean(tpe: jdi.Type): Boolean =
    tpe.isInstanceOf[jdi.BooleanType] || tpe.name == "java.lang.Boolean"

  private def isMutable(tree: RuntimeEvaluationTree): Boolean =
    tree match {
      case field: Field => !field.immutable
      case localVar: LocalVar => true
      case _ => false
    }

  private def validateAssign(tree: Term.Assign): Validation[RuntimeEvaluationTree] = {
    val lhs =
      tree.lhs match {
        case Term.Select(qual, Name(name)) =>
          validateAsValue(qual)
            .flatMap(findField(_, name, false))
            .orElse(validateAsClass(qual).flatMap(findStaticField(_, name, false)))
        case Term.Name(name) =>
          findVariable(name, false)
            .orElse(thisTree.flatMap(findField(_, name, false)))
            .orElse(declaringType.flatMap(findStaticField(_, name, false)))
        case _ => CompilerRecoverable("Unsupported assignment")
      }

    for {
      lhsValue <- lhs
      if isMutable(lhsValue)
      rhs <- validateAsValue(tree.rhs)
      if isAssignableFrom(rhs.`type`, lhsValue.`type`)
      unit <- unitTree
      assign <- Assign(lhsValue, rhs, unit.`type`)
    } yield assign
  }

  private def asReference(tpe: jdi.Type): Validation[jdi.ReferenceType] =
    tpe match {
      case tpe: jdi.ReferenceType => Valid(tpe)
      case _ => Recoverable(s"$tpe is not a reference type")
    }

  private def fromLitToValue(literal: Lit, classLoader: JdiClassLoader): (Safe[Any], jdi.Type) = {
    val tpe = classLoader
      .mirrorOfLiteral(literal.value)
      .map(_.value.`type`)
      .getResult
      .get

    (Safe(literal.value), tpe)
  }

  private def moreSpecificThan(m1: jdi.Method, m2: jdi.Method): Boolean = {
    m1.argumentTypes()
      .asScala
      .zip(m2.argumentTypes().asScala)
      .forall {
        case (t1, t2) if t1.name == t2.name => true
        case (_: jdi.PrimitiveType, _) => true
        case (_, _: jdi.PrimitiveType) => true
        case (r1: jdi.ReferenceType, r2: jdi.ReferenceType) => isAssignableFrom(r1, r2)
      }
  }

  private def argsMatch(method: jdi.Method, args: Seq[jdi.Type], boxing: Boolean): Boolean =
    method.argumentTypeNames().size() == args.size && areAssignableFrom(method, args, boxing)

  /**
   * @see <a href="https://docs.oracle.com/javase/specs/jls/se20/html/jls-15.html#jls-15.12.2.5">JLS#15.12.2.5. Choosing the most specific method</a>
   *
   * @param methods the list of compatible methods to compare
   * @return a sequence containing the most precise methods
   */
  private def filterMostPreciseMethod(methods: Iterable[jdi.Method]): Seq[jdi.Method] =
    methods.foldLeft(List.empty[jdi.Method]) { (m1, m2) =>
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
   * If multiple methods are found, a [[Validation.Unrecoverable]] is returned
   *
   * It loads and prepares the method return type, if needed.
   *
   * @param ref the reference type on which to look for the method
   * @param encodedName the name of the method
   * @param args the arguments types of the method
   * @return the method, wrapped in a [[Validation]]
   */
  private def findMethodBySignedName(
      ref: jdi.ReferenceType,
      name: String,
      args: Seq[jdi.Type]
  ): Validation[jdi.Method] = {
    val candidates = findMethodsByName(ref, name)
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
      case _ => filterMostPreciseMethod(withoutBridges)
    }
    finalCandidates
      .validateSingle(s"Cannot find a proper method $name with args types $args on $ref")
      .map(loadClassOnNeed)
  }

  private def findZeroArgMethod(qualifier: RuntimeEvaluationTree, name: String): Validation[CallMethod] =
    asReference(qualifier.`type`)
      .flatMap(zeroArgMethodByName(_, name))
      .flatMap { m =>
        if (isModuleCall(m))
          Recoverable("Accessing a module from its instanciation method is not allowed")
        else asInstanceMethod(m, Seq.empty, qualifier)
      }

  private def findZeroArgStaticMethod(qualifier: jdi.ReferenceType, name: String): Validation[CallMethod] =
    zeroArgMethodByName(qualifier, name)
      .flatMap { m =>
        if (isModuleCall(m))
          Recoverable("Accessing a module from its instanciation method is not allowed")
        else asStaticMethod(m, Seq.empty, qualifier)
      }

  private def isModuleCall(m: jdi.Method): Boolean = {
    val rt = m.returnTypeName
    val noArgs = m.argumentTypeNames.size == 0
    val isSingleton = rt.endsWith("$")
    val isSingletonInstantiation = rt.stripSuffix("$").endsWith(m.name)
    noArgs && isSingleton && isSingletonInstantiation
  }

  private def zeroArgMethodByName(ref: jdi.ReferenceType, name: String): Validation[jdi.Method] = {
    findMethodsByName(ref, name).filter(_.argumentTypeNames.isEmpty) match {
      case Seq() => Recoverable(s"Cannot find a proper method $name with no args on $ref")
      case Seq(method) => Valid(method).map(loadClassOnNeed)
      case methods =>
        methods
          .filterNot(_.isBridge())
          .validateSingle(s"Cannot find a proper method $name with no args on $ref")
          .map(loadClassOnNeed)
    }
  }

  private def findMethodsByName(ref: jdi.ReferenceType, name: String): Seq[jdi.Method] = {
    val encodedName = if (name == "<init>" && ref.isInstanceOf[jdi.ClassType]) name else NameTransformer.encode(name)
    ref.methodsByName(encodedName).asScala.toSeq
  }

  private def findMethodBySignedName(
      qualifier: RuntimeEvaluationTree,
      name: String,
      args: Seq[RuntimeEvaluationTree]
  ): Validation[CallMethod] = {
    if (!args.isEmpty) {
      asReference(qualifier.`type`)
        .flatMap(tpe => findMethodBySignedName(tpe, name, args.map(_.`type`)))
        .flatMap(asInstanceMethod(_, args, qualifier))
    } else findZeroArgMethod(qualifier, name)
  }

  private def findStaticMethodBySignedName(
      qualifier: jdi.ReferenceType,
      name: String,
      args: Seq[RuntimeEvaluationTree]
  ): Validation[CallMethod] =
    if (!args.isEmpty)
      findMethodBySignedName(qualifier, name, args.map(_.`type`)).flatMap(asStaticMethod(_, args, qualifier))
    else findZeroArgStaticMethod(qualifier, name)

  private def needsOuter(cls: jdi.ReferenceType): Boolean =
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

  private def newInstanceTreeByArgs(cls: jdi.ReferenceType, args: Seq[RuntimeEvaluationTree]): Validation[NewInstance] =
    findMethodBySignedName(cls, "<init>", args.map(_.`type`))
      .map(m => NewInstance(CallStaticMethod(m, args, cls)))

  private def isAssignableFrom(got: jdi.Type, expected: jdi.Type): Boolean = {
    def referenceTypesMatch(got: jdi.ReferenceType, expected: jdi.ReferenceType) = {
      val assignableFrom = expected.classObject().referenceType().methodsByName("isAssignableFrom").get(0)
      val params = Seq(got.classObject()).asJava
      expected.classObject
        .invokeMethod(frame.thread, assignableFrom, params, jdi.ObjectReference.INVOKE_SINGLE_THREADED)
        .asInstanceOf[jdi.BooleanValue]
        .value()
    }

    (got, expected) match {
      case (g: jdi.ArrayType, at: jdi.ArrayType) =>
        checkClassStatus(at.componentType())
        g.componentType().equals(at.componentType())
      case (g: jdi.PrimitiveType, pt: jdi.PrimitiveType) => got.equals(pt)
      case (g: jdi.ReferenceType, ref: jdi.ReferenceType) => referenceTypesMatch(g, ref)
      case (_: jdi.VoidType, _: jdi.VoidType) => true
      case (g: jdi.ReferenceType, pt: jdi.PrimitiveType) =>
        isAssignableFrom(g, frame.getPrimitiveBoxedClass(pt))
      case (g: jdi.PrimitiveType, ct: jdi.ReferenceType) =>
        isAssignableFrom(frame.getPrimitiveBoxedClass(g), ct)

      case _ => false
    }
  }

  private def areAssignableFrom(method: jdi.Method, args: Seq[jdi.Type], boxing: Boolean): Boolean =
    if (method.argumentTypes().size() != args.size) false
    else
      method
        .argumentTypes()
        .asScala
        .zip(args)
        .forall {
          case (_: jdi.PrimitiveType, _: jdi.ReferenceType) if !boxing => false
          case (_: jdi.ReferenceType, _: jdi.PrimitiveType) if !boxing => false
          case (expected, got) => isAssignableFrom(got, expected)
        }

  private def loadClass(name: String): Validation[jdi.ClassType] =
    Validation.fromTry(frame.classLoader().flatMap(_.loadClass(name)).extract(_.cls))

  private def checkClassStatus(tpe: => jdi.Type) = Try(tpe) match {
    case Failure(e: jdi.ClassNotLoadedException) => loadClass(e.className())
    case Success(value: jdi.ClassType) if !value.isPrepared => loadClass(value.name)
    case result => result
  }

  private def loadClassOnNeed[T <: jdi.TypeComponent](tc: T): T = {
    def tpe = tc match {
      case field: jdi.Field => field.`type`
      case method: jdi.Method => method.returnType
    }
    val name = tc match {
      case field: jdi.Field => field.typeName
      case method: jdi.Method => method.returnTypeName
    }
    checkClassStatus(tpe)
    tc
  }

  // ! TO REFACTOR :sob:
  private def resolveInnerType(qualifier: jdi.Type, name: String): Validation[jdi.ReferenceType] = {
    var current: Validation[jdi.ReferenceType] = Recoverable(s"Cannot find outer class for $qualifier")
    def loop(qualifier: jdi.Type): Validation[jdi.ReferenceType] =
      qualifier match {
        case _: jdi.ArrayType | _: jdi.PrimitiveType | _: jdi.VoidType =>
          Recoverable("Cannot find outer class on non reference type")
        case ref: jdi.ReferenceType =>
          val loadedCls = loadClass(concatenateInnerTypes(ref.name, name))
          if (loadedCls.isValid) loadedCls
          else {
            var superTypes: List[jdi.ReferenceType] = ref match {
              case cls: jdi.ClassType => cls.superclass() :: cls.interfaces().asScala.toList
              case itf: jdi.InterfaceType => itf.superinterfaces().asScala.toList
            }

            while (!superTypes.isEmpty && !current.isValid) {
              val res = loop(superTypes.head)
              if (res.isValid) current = res
              else superTypes = superTypes.tail
            }
            current
          }
      }

    loop(qualifier)
  }

  private def getCommonSuperClass(tpe1: jdi.Type, tpe2: jdi.Type): Option[jdi.Type] = {
    def getSuperClasses(of: jdi.Type): Array[jdi.ClassType] =
      of match {
        case cls: jdi.ClassType =>
          Iterator.iterate(cls)(cls => cls.superclass()).takeWhile(_ != null).toArray
        case _ => Array()
      }

    val superClasses1 = getSuperClasses(tpe1)
    val superClasses2 = getSuperClasses(tpe2)
    superClasses1.find(superClasses2.contains)
  }

  private def validateType(
      tpe: scala.meta.Type,
      thisTree: Option[RuntimeEvaluationTree]
  ): Validation[(Option[RuntimeEvaluationTree], jdi.ReferenceType)] =
    tpe match {
      case scala.meta.Type.Name(name) =>
        // won't work if the class is defined in one of the outer of this
        findClass(name).map(cls => (thisTree, cls))
      case scala.meta.Type.Select(qual, name) =>
        val cls = for {
          qual <- validateAsValue(qual)
          tpe <- resolveInnerType(qual.`type`, name.value)
        } yield
          if (tpe.isStatic()) (None, tpe)
          else (Some(qual), tpe)
        cls.orElse {
          findQualifiedClass(name.value, qual.toString).map(cls => (None, cls))
        }
      case _ => Recoverable("Type not supported at runtime")
    }

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

  private def asStaticModule(tpe: jdi.Type): Validation[RuntimeEvaluationTree] =
    tpe match {
      case tpe: jdi.ClassType if tpe.fieldByName("MODULE$") != null => preEvaluate(StaticModule(tpe))
      case _ => Recoverable(s"$tpe is not a static module class")
    }

  private def asModule(tpe: jdi.ReferenceType, qualifier: RuntimeEvaluationTree): Validation[RuntimeEvaluationTree] =
    asStaticModule(tpe).orElse {
      qualifier.`type` match {
        case ref: jdi.ReferenceType =>
          val objectName = NameTransformer.scalaClassName(tpe.name).stripSuffix("$")
          zeroArgMethodByName(ref, objectName)
            .map(m => NestedModule(tpe, CallInstanceMethod(m, Seq.empty, qualifier)))
            .flatMap(preEvaluate)
        case _ => Recoverable(s"Cannot find module initializer for non-reference type $tpe")
      }
    }

  private def asInstanceField(
      field: jdi.Field,
      qualifier: RuntimeEvaluationTree,
      preevaluate: Boolean
  ): Validation[RuntimeEvaluationTree] = {
    asStaticModule(field.`type`)
      .orElse {
        asStaticModule(qualifier.`type`)
          .map(_ => InstanceField(field, qualifier))
          .orElse {
            if (field.isStatic())
              Recoverable(
                s"Accessing static field $field from instance of ${qualifier.`type`} can have unexpected behavior"
              )
            else Valid(InstanceField(field, qualifier))
          }
          .flatMap(instanceField => if (preevaluate) preEvaluate(instanceField) else Valid(instanceField))
      }
  }

  private def asStaticField(field: jdi.Field, preevaluate: Boolean): Validation[RuntimeEvaluationTree] =
    if (field.isStatic) {
      val staticField = StaticField(field)
      if (preevaluate) preEvaluate(staticField) else Valid(staticField)
    } else Recoverable(s"Cannot access instance field $field from static context")

  private def asInstanceMethod(
      method: jdi.Method,
      args: Seq[RuntimeEvaluationTree],
      qualifier: RuntimeEvaluationTree
  ): Validation[CallMethod] =
    if (method.isStatic())
      Recoverable(s"Accessing static method $method from instance of ${qualifier.`type`} can have unexpected behavior")
    else Valid(CallInstanceMethod(method, args, qualifier))

  private def asStaticMethod(
      method: jdi.Method,
      args: Seq[RuntimeEvaluationTree],
      qualifier: jdi.ReferenceType
  ): Validation[CallMethod] =
    if (method.isStatic()) Valid(CallStaticMethod(method, args, qualifier))
    else Recoverable(s"Cannot access instance method $method from static context")

  private implicit class IterableExtensions[A](iter: Iterable[A]) {
    def validateSingle(message: String): Validation[A] =
      iter.size match {
        case 1 => Valid(iter.head)
        case 0 => Recoverable(message)
        case _ => CompilerRecoverable(s"$message: multiple values found")
      }
  }
}
