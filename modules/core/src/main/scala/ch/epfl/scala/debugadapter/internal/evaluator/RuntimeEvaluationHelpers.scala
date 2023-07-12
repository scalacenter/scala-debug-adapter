package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._

import scala.meta.Lit
import scala.meta.Stat
import scala.meta.Term
import scala.meta.trees.*
import scala.meta.{Type => MType}
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.jdk.CollectionConverters.*

import RuntimeEvaluatorExtractors.*
import ch.epfl.scala.debugadapter.Logger

private[evaluator] class RuntimeEvaluationHelpers(frame: JdiFrame)(implicit logger: Logger) {
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

  def moreSpecificThan(m1: Method, m2: Method): Boolean = {
    m1.argumentTypes()
      .asScalaSeq
      .zip(m2.argumentTypes().asScalaSeq)
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
  private def extractMostPreciseMethod(methods: Seq[Method]): Seq[Method] =
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
    val candidates: List[Method] = ref.methodsByName(encodedName).asScalaList

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
      .toValidation(s"Cannot find a proper method $encodedName with args types $args on $ref")
      .map(loadClassOnNeed)
  }

  def methodTreeByNameAndArgs(
      tree: RuntimeTree,
      funName: String,
      args: Seq[RuntimeEvaluableTree]
  ): Validation[MethodTree] = tree match {
    case ReferenceTree(ref) =>
      methodsByNameAndArgs(ref, NameTransformer.encode(funName), args.map(_.`type`)).flatMap {
        case ModuleCall() =>
          Recoverable("Accessing a module from its instanciation method is not allowed at console-level")
        case mt => toStaticIfNeeded(mt, args, tree)
      }
    case _ => Recoverable(new IllegalArgumentException(s"Cannot find method $funName on $tree"))
  }

  def needsOuter(cls: ClassType): Boolean =
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

  def newInstanceTreeByArgs(cls: ClassType, args: Seq[RuntimeEvaluableTree]): Validation[NewInstanceTree] =
    methodsByNameAndArgs(cls, "<init>", args.map(_.`type`))
      .map(m => NewInstanceTree(StaticMethodTree(m, args, cls)))

  /* -------------------------------------------------------------------------- */
  /*                                Type checker                                */
  /* -------------------------------------------------------------------------- */
  def isAssignableFrom(got: Type, expected: Type): Boolean = {
    def referenceTypesMatch(got: ReferenceType, expected: ReferenceType) = {
      val assignableFrom = expected.classObject().referenceType().methodsByName("isAssignableFrom").get(0)
      val params = Seq(got.classObject()).asJavaList
      expected.classObject
        .invokeMethod(frame.thread, assignableFrom, params, ObjectReference.INVOKE_SINGLE_THREADED)
        .asInstanceOf[BooleanValue]
        .value()
    }

    (got, expected) match {
      case (g: ArrayType, at: ArrayType) => g.componentType().equals(at.componentType()) // TODO: check this
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

  def areAssignableFrom(method: Method, args: Seq[Type], boxing: Boolean): Boolean =
    if (method.argumentTypes().size() != args.size) false
    else
      method
        .argumentTypes()
        .asScalaSeq
        .zip(args)
        .forall {
          case (_: PrimitiveType, _: ReferenceType) | (_: ReferenceType, _: PrimitiveType) if !boxing => false
          case (expected, got) =>
            isAssignableFrom(got, expected)
        }

  /* -------------------------------------------------------------------------- */
  /*                                Class helpers                               */
  /* -------------------------------------------------------------------------- */
  def loadClass(name: String): Safe[JdiClass] =
    frame.classLoader().flatMap(_.loadClass(name))

  def checkClassStatus(tpe: => Type)(name: String) = Try(tpe) match {
    case Failure(_: ClassNotLoadedException) => loadClass(name).extract(_.cls)
    case Success(value: ClassType) if !value.isPrepared => loadClass(name).extract(_.cls)
    case result => result
  }

  def loadClassOnNeed[T <: TypeComponent](tc: T): T = {
    def tpe = tc match {
      case field: Field => field.`type`
      case method: Method => method.returnType
    }
    val name = tc match {
      case field: Field => field.typeName()
      case method: Method => method.returnTypeName()
    }
    checkClassStatus(tpe)(name)
    tc
  }

  // ! TO REFACTOR :sob:
  def resolveInnerType(qual: Type, name: String) = {
    var tpe: Validation[ClassType] = Recoverable(s"Cannot find outer class for $qual")
    def loop(on: Type): Validation[ClassType] =
      on match {
        case _: ArrayType | _: PrimitiveType | _: VoidType =>
          Recoverable("Cannot find outer class on non reference type")
        case ref: ReferenceType =>
          val loadedCls = loadClass(concatenateInnerTypes(ref.name, name)).extract(_.cls)
          if (loadedCls.isSuccess) Valid(loadedCls.get)
          else {
            var superTypes: List[ReferenceType] = ref match {
              case cls: ClassType => cls.superclass() :: cls.interfaces().asScalaList
              case itf: InterfaceType => itf.superinterfaces().asScalaList
            }

            while (!superTypes.isEmpty && tpe.isInvalid) {
              val res = loop(superTypes.head)
              if (res.isValid) tpe = res
              else superTypes = superTypes.tail
            }
            tpe
          }
      }

    loop(qual)
  }

  def extractCommonSuperClass(tpe1: Type, tpe2: Type): Option[Type] = {
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

  def validateType(tpe: MType, thisTree: Option[RuntimeEvaluableTree])(
      termValidation: Term => Validation[RuntimeEvaluableTree]
  ): Validation[(Option[RuntimeEvaluableTree], ClassTree)] =
    tpe match {
      case MType.Name(name) =>
        // won't work if the class is defined in one of the outer of this
        searchAllClassesFor(name, thisTree.map(_.`type`.name)).map(cls => (thisTree, cls))
      case MType.Select(qual, name) =>
        val cls = for {
          qual <- termValidation(qual)
          tpe <- resolveInnerType(qual.`type`, name.value)
        } yield
          if (tpe.isStatic()) (None, ClassTree(tpe))
          else (Some(qual), ClassTree(tpe))
        cls.orElse(searchAllClassesFor(qual.toString + "." + name.value, thisTree.map(_.`type`.name)).map((None, _)))
      case _ => Recoverable("Type not supported at runtime")
    }

  // ! May not be correct when dealing with an object inside a class
  def outerLookup(ref: ReferenceType): Validation[Type] =
    Validation(ref.fieldByName("$outer"))
      .map(_.`type`())
      .orElse {
        // outer static object
        removeLastInnerTypeFromFQCN(ref.name())
          .map(name => loadClass(name + "$").extract) match {
          case Some(Success(Module(mod))) => Valid(mod)
          case _ => Recoverable(s"Cannot find $$outer for $ref")
        }
      }

  def searchAllClassesFor(name: String, in: Option[String]): Validation[ClassTree] = {
    def fullName = in match {
      case Some(value) if value == name => name // name duplication when indirect apply call
      case Some(value) => concatenateInnerTypes(value, name)
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
        .filter(cls => cls.name() == name || nameEndMatch(cls.name()))

    def finalCandidates =
      candidates.size match {
        case 0 =>
          val topLevelClassName = frame.thisObject
            .map(_.reference.referenceType.name)
            .map(_.split('.').init.mkString(".") + "." + name)
            .getOrElse("")
          loadClass(fullName)
            .orElse(loadClass(topLevelClassName))
            .extract(_.cls)
            .toSeq
        case 1 => candidates
        case _ => candidates.filter(_.name() == fullName)
      }

    finalCandidates
      .toValidation(s"Cannot find module/class $name, has it been loaded ?")
      .map(cls => ClassTree(checkClassStatus(cls)(cls.name()).get.asInstanceOf[ClassType]))
  }

  /* -------------------------------------------------------------------------- */
  /*                              Initialize module                             */
  /* -------------------------------------------------------------------------- */
  def initializeModule(modCls: ClassType, evaluated: Safe[JdiValue]): Safe[JdiValue] = {
    for {
      ofValue <- evaluated
      initMethodName <- Safe(getLastInnerType(modCls.name()).get)
      instance <- ofValue.value.`type` match {
        case module if module == modCls => Safe(ofValue)
        case _ => ofValue.asObject.invoke(initMethodName, Seq.empty)
      }
    } yield instance
  }
  def illegalAccess(x: Any, typeName: String) = Fatal {
    new ClassCastException(s"Cannot cast $x to $typeName")
  }

  /* -------------------------------------------------------------------------- */
  /*                               Useful patterns                              */
  /* -------------------------------------------------------------------------- */
  /* Extract reference if there is */
  def extractReferenceType(tree: Validation[RuntimeTree]): Validation[ReferenceType] =
    tree.flatMap(extractReferenceType)

  def extractReferenceType(tree: RuntimeTree): Validation[ReferenceType] =
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
  /*                           Nested types regex                          */
  /* -------------------------------------------------------------------------- */
  def getLastInnerType(className: String): Option[String] = {
    val pattern = """(.+\$)([^$]+)$""".r
    className.stripSuffix("$") match {
      case pattern(_, innerType) => Some(innerType)
      case _ => None
    }
  }

  def removeLastInnerTypeFromFQCN(className: String): Option[String] = {
    val pattern = """(.+)\$[\w]+\${0,1}$""".r
    className match {
      case pattern(baseName) => Some(baseName)
      case _ => None
    }
  }

  def concatenateInnerTypes(className: String, innerName: String): String =
    if (className.endsWith("$")) className + innerName
    else className + "$" + innerName

  /* -------------------------------------------------------------------------- */
  /*                  Transformation to static or instance tree                 */
  /* -------------------------------------------------------------------------- */
  def toStaticIfNeeded(field: Field, on: RuntimeTree): Validation[RuntimeEvaluableTree] =
    (field.`type`, on) match {
      case (Module(module), _) => Valid(TopLevelModuleTree(module))
      case (_, cls: ClassTree) => Valid(StaticFieldTree(field, cls.`type`))
      case (_, Module(mod)) => Valid(InstanceFieldTree(field, mod))
      case (_, eval: RuntimeEvaluableTree) =>
        if (field.isStatic())
          CompilerRecoverable(
            s"Accessing static field $field from instance ${eval.`type`} can lead to unexpected behavior"
          )
        else Valid(InstanceFieldTree(field, eval))
    }

  def toStaticIfNeeded(
      method: Method,
      args: Seq[RuntimeEvaluableTree],
      on: RuntimeTree
  ): Validation[MethodTree] = on match {
    case cls: ClassTree => Valid(StaticMethodTree(method, args, cls.`type`))
    case Module(mod) => Valid(InstanceMethodTree(method, args, mod))
    case eval: RuntimeEvaluableTree =>
      if (method.isStatic())
        CompilerRecoverable(
          s"Accessing static method $method from instance ${eval.`type`} can lead to unexpected behavior"
        )
      else Valid(InstanceMethodTree(method, args, eval))
  }
}
