package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._
import scala.meta.trees.*
import scala.meta.Lit
import scala.util.Success
import RuntimeEvaluatorExtractors.*
import scala.meta.Stat
import scala.meta.Term
import scala.util.Failure
import scala.util.Try

private[evaluator] object Helpers {
  def illegalAccess(x: Any, typeName: String) = Fatal {
    new ClassCastException(s"Cannot cast $x to $typeName")
  }

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
  private implicit class RichMethod(m: Method) {
    private def refTypeDistance(from: ReferenceType, to: ReferenceType) = {
      def loop(from: ReferenceType, acc: Int): Option[Int] = {
        val fromSuperClass = Option(from.asInstanceOf[ClassType].superclass())
        lazy val fromInterfaces: Seq[InterfaceType] =
          from.asInstanceOf[ClassType].interfaces().asScalaSeq

        fromSuperClass
          .flatMap { spr =>
            if (spr.name() == to.name()) Some(acc + 1)
            else loop(spr, acc + 1)
          }
          .orElse {
            fromInterfaces
              .find { itf =>
                if (itf.name() == to.name()) true
                else loop(itf, acc + 1).isDefined
              }
              .map(_ => acc + 1)
          }
      }
      loop(from, 0)
    }

    def moreSpecificThan(m2: Method): Boolean = {
      println(s"\u001b[31mComparing ${m} and ${m2}\u001b[0m")
      m.argumentTypes()
        .asScalaSeq
        .zip(m2.argumentTypes().asScalaSeq)
        .forall {
          case (t1, t2) if t1.name == t2.name => true
          case (_: PrimitiveType, _) => true
          case (_, _: PrimitiveType) => true
          case (r1: ReferenceType, r2: ReferenceType) => refTypeDistance(r1, r2).isDefined
        }
    }
  }

  private def argsMatch(method: Method, args: Seq[Type], frame: JdiFrame): Boolean =
    method.argumentTypeNames().size() == args.size && areAssignableFrom(method, args, frame)

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
              if (m.moreSpecificThan(m2)) List(m)
              else if (m2.moreSpecificThan(m)) List(m2)
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

    val withoutBridges = candidates.size match {
      case 0 | 1 => candidates
      case _ => candidates.filterNot(_.isBridge())
    }

    val finalCandidates = withoutBridges.size match {
      case 0 | 1 => withoutBridges
      case _ => extractMostPreciseMethod(withoutBridges)
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
    if (method.argumentTypes().size() != args.size) false
    else
      method
        .argumentTypes()
        .asScalaSeq
        .zip(args)
        .forall { case (expected, got) =>
          isAssignableFrom(got, expected, frame)
        }

  /* -------------------------------------------------------------------------- */
  /*                             Looking for $outer                             */
  /* -------------------------------------------------------------------------- */
  def findOuter(tree: RuntimeTree, frame: JdiFrame): Validation[OuterTree] = {
    def outerLookup(ref: ReferenceType) = Validation(ref.fieldByName("$outer")).map(_.`type`()).orElse {
      removeLastInnerTypeFromFQCN(ref.name())
        .map(name => loadClass(name + "$", frame)) match {
        case Some(Safe(Success(Module(mod: ClassType)))) => Valid(mod)
        case _ => Recoverable(s"Cannot find $$outer for $ref")
      }
    }

    for {
      ref <- ifReference(tree)
      outer <- outerLookup(ref)
      outerTree <- OuterTree(tree, outer)
    } yield outerTree
  }

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

  /* -------------------------------------------------------------------------- */
  /*                               Useful patterns                              */
  /* -------------------------------------------------------------------------- */
  /* Extract reference if there is */
  def ifReference(tree: Validation[RuntimeTree]): Validation[ReferenceType] =
    tree match {
      case Valid(tree) => ifReference(tree)
      case e: Invalid => Recoverable(s"Invalid reference: $e")
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

  def removeLastInnerTypeFromFQCN(className: String): Option[String] = {
    val pattern = """(.+)\$[\w]+\${0,1}$""".r
    className match {
      case pattern(baseName) => Some(baseName)
      case _ => None
    }
  }

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
          Fatal(s"Accessing static field $field from instance ${eval.`type`} can lead to unexpected behavior")
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
        Fatal(s"Accessing static method $method from instance ${eval.`type`} can lead to unexpected behavior")
      else Valid(InstanceMethodTree(method, args, eval))
  }

  /* -------------------------------------------------------------------------- */
  /*                                Class helpers                               */
  /* -------------------------------------------------------------------------- */
  def loadClass(name: String, frame: JdiFrame): Safe[JdiClass] =
    frame.classLoader().flatMap(_.loadClass(name))

  def checkClassStatus(tpe: => Type)(name: String, frame: JdiFrame) = Try(tpe) match {
    case Failure(_: ClassNotLoadedException) => loadClass(name, frame).extract(_.cls)
    case Success(value: ClassType) if !value.isPrepared => loadClass(name, frame).extract(_.cls)
    case result => result
  }

  def loadClassOnNeed[T <: TypeComponent](tc: T, frame: JdiFrame): T = {
    checkClassStatus(tc.`type`)(tc.typeName, frame)
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
          loadClass(fullName, frame)
            .orElse { loadClass(topLevelClassName, frame) }
            .extract(_.cls)
            .toSeq
        case 1 => candidates
        case _ => candidates.filter(_.name() == fullName)
      }

    finalCandidates
      .toValidation(s"Cannot find module/class $name, has it been loaded ?")
      .map { cls => checkClassStatus(cls)(cls.name(), frame).get.asInstanceOf[ClassType] }
  }
}
