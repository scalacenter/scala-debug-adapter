package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.internal.scalasig._
import com.microsoft.java.debug.core.adapter.{
  StepFilterProvider => JavaStepFilterProvider
}
import com.microsoft.java.debug.core.protocol.Requests.StepFilters
import com.sun.jdi
import com.sun.jdi.AbsentInformationException

import scala.collection.JavaConverters._
import ch.epfl.scala.debugadapter.Logger

class StepFilterProvider(
    sourceLookUp: SourceLookUpProvider,
    scalaVersion: String,
    logger: Logger
) extends JavaStepFilterProvider() {

  override def skip(method: jdi.Method, filters: StepFilters): Boolean = {
    try {
      if (method.isBridge) true
      else if (isDynamicClass(method.declaringType)) true
      else if (super.skip(method, filters)) true
      else if (isJava(method)) false
      else if (isLocalMethod(method)) false
      else if (isLocalClass(method.declaringType)) false
      else if (isDefaultValue(method)) false
      else if (isLazyInitializer(method)) skipLazyInitializer(method)
      else skipScalaMethod(method)
    } catch {
      case e: Exception =>
        logger.error(
          s"Failed to determine if ${method} should be skipped: ${e.getMessage}"
        )
        logger.trace(e)
        false
    }
  }

  private def isDynamicClass(tpe: jdi.ReferenceType): Boolean =
    try {
      tpe.sourceName()
      false
    } catch {
      case _: AbsentInformationException =>
        // We assume that a ReferenceType with no source name is necessarily a dynamic class
        true
    }

  private def isJava(method: jdi.Method): Boolean =
    method.declaringType.sourceName.endsWith(".java")

  private def isLocalMethod(method: jdi.Method): Boolean =
    method.name.contains("$anonfun$")

  private def isDefaultValue(method: jdi.Method): Boolean =
    method.name.contains("$default$")

  private def isLocalClass(tpe: jdi.ReferenceType): Boolean =
    tpe.name.contains("$anon$")

  private def isLazyInitializer(method: jdi.Method): Boolean =
    method.name.endsWith("$lzycompute")

  private def skipLazyInitializer(method: jdi.Method): Boolean = {
    val fieldName = method.name.dropRight(11)
    method.declaringType match {
      case cls: jdi.ClassType =>
        cls.allInterfaces.asScala.exists(interface =>
          containsLazyField(interface, fieldName)
        )
      case t =>
        logger.warn(
          s"Expected declaring type of $method to be a class, found ${t.getClass.getSimpleName}"
        )
        false
    }
  }

  private def skipScalaMethod(method: jdi.Method): Boolean = {
    val fqcn = method.declaringType.name
    val matchingMethods = for {
      scalaSig <- sourceLookUp.getScalaSig(fqcn).toSeq
      val isObject = fqcn.endsWith("$")
      scalaMethod <- scalaSig.entries.toSeq
        .collect { case m: MethodSymbol if m.isMethod => m }
      if scalaMethod.parent.exists(p => p.isModule == isObject)
      if matchSymbol(method, scalaMethod)
    } yield scalaMethod

    if (matchingMethods.size > 1) {
      val builder = new java.lang.StringBuilder
      builder.append(
        s"Found ${matchingMethods.size} matching symbols for $method:\n"
      )
      val printer = new ScalaSigPrinter(builder)
      matchingMethods.foreach(printer.printSymbol)
      logger.warn(builder.toString)
    }

    val res = matchingMethods.headOption.forall(skip)

    if (res) {
      logger.debug(s"Skipping $method")
    }

    res
  }

  private def containsLazyField(
      interface: jdi.InterfaceType,
      fieldName: String
  ): Boolean = {
    val fqcn = interface.name
    sourceLookUp.getScalaSig(fqcn).exists(containsLazyField(_, fieldName))
  }

  private def containsLazyField(
      scalaSig: ScalaSig,
      fieldName: String
  ): Boolean = {
    scalaSig.entries.exists {
      case m: MethodSymbol => m.isLazy && m.name == fieldName
      case _ => false
    }
  }

  private def skip(scalaMethod: MethodSymbol): Boolean = {
    scalaMethod.isSynthetic ||
    // we skip if it is an accessor, except if it is the accessor of a lazy field in a trait
    // because the accessor of a lazy field in a trait is its initializer
    (scalaMethod.isAccessor && (!scalaMethod.isLazy || !scalaMethod.parent.get.isTrait))
  }

  private def matchSymbol(
      javaMethod: jdi.Method,
      scalaMethod: MethodSymbol
  ): Boolean = {
    // TODO find what is an aliasRef
    if (scalaMethod.aliasRef.nonEmpty)
      logger.debug(
        s"aliasRef for ${scalaMethod.name}: ${scalaMethod.aliasRef}"
      )

    javaMethod.name == scalaMethod.name &&
    matchOwner(javaMethod.declaringType, scalaMethod.parent.get) &&
    matchSignature(javaMethod, scalaMethod.infoType)
  }

  private def matchOwner(
      javaClass: jdi.ReferenceType,
      scalaClass: Symbol
  ): Boolean = {
    getOwners(scalaClass)
      .foldRight(Option(javaClass.name)) { (sym, acc) =>
        for (javaName <- acc if javaName.contains(sym.name)) yield {
          javaName
            .split(sym.name)
            .drop(1)
            .mkString(sym.name)
        }
      }
      .exists { remainder =>
        remainder.forall(c => c.isDigit || c == '$')
      }
  }

  private def getOwners(sym: Symbol): Seq[Symbol] = {
    Iterator
      .iterate(Option(sym))(opt => opt.flatMap(_.parent))
      .takeWhile(_.isDefined)
      .flatten
      .toSeq
  }

  private def matchSignature(
      javaMethod: jdi.Method,
      methodType: Type
  ): Boolean = {
    val (scalaArgs, scalaReturnType) = extractParametersAndReturnType(
      methodType
    )
    def matchAllArguments: Boolean = {
      val javaArgs = javaMethod.arguments.asScala.toSeq
      javaArgs.size == scalaArgs.size &&
      javaArgs
        .zip(scalaArgs)
        .forall { case (javaArg, scalaArg) =>
          matchArgument(javaArg, scalaArg, javaMethod.declaringType)
        }
    }
    def matchReturnType: Boolean = {
      javaMethod.isConstructor ||
      matchType(
        javaMethod.returnType,
        scalaReturnType,
        javaMethod.declaringType
      )
    }
    matchAllArguments && matchReturnType
  }

  private[internal] def extractParametersAndReturnType(
      methodType: Type
  ): (Seq[Symbol], Type) = {
    methodType match {
      case m: FunctionType =>
        val (params, returnType) = extractParametersAndReturnType(
          m.resultType.get
        )
        (m.paramSymbols ++ params, returnType)
      case m: NullaryMethodType => (Seq.empty, m.resultType.get)
      case m: PolyType => extractParametersAndReturnType(m.typeRef.get)
      case returnType => (Seq.empty, returnType)
    }
  }

  private def matchArgument(
      javaArg: jdi.LocalVariable,
      scalaArg: Symbol,
      declaringType: jdi.Type
  ): Boolean = {
    val scalaType = scalaArg.asInstanceOf[MethodSymbol].infoType
    javaArg.name == scalaArg.name &&
    (
      // we cannot check the type of the `this` argument in methods of value classes
      scalaArg.name == "$this" ||
        matchType(javaArg.`type`, scalaType, declaringType)
    )
  }

  private def matchType(
      javaType: jdi.Type,
      scalaType: Type,
      declaringType: jdi.Type
  ): Boolean = {
    scalaType match {
      case ThisType(sym) => javaType == declaringType
      case SingleType(typeRef, sym) =>
        matchTypeSymbol(javaType, sym, declaringType)
      case ConstantType(constant) =>
        val expectedType = constant.get.value match {
          case name: Ref[_] => "java.lang.String"
          case value =>
            value.getClass.getName match {
              case "java.lang.Boolean" => "boolean"
              case "java.lang.Byte" => "byte"
              case "java.lang.Character" => "char"
              case "java.lang.Double" => "double"
              case "java.lang.Float" => "float"
              case "java.lang.Integer" => "int"
              case "java.lang.Long" => "long"
              case "java.lang.Short" => "short"
            }
        }
        javaType.name == expectedType
      case TypeRefType(prefix, sym, typeArgs) =>
        matchTypeSymbol(javaType, sym, declaringType)
      case RefinedType(sym, typeRefs) =>
        typeRefs.headOption
          .map(matchType(javaType, _, declaringType))
          .getOrElse(javaType.name == "java.lang.Object")
      case AnnotatedType(typeRef) =>
        matchType(javaType, typeRef.get, declaringType)
      case ExistentialType(typeRef, paramRefs) =>
        matchType(javaType, typeRef.get, declaringType)
      case other =>
        logger.warn(s"Unexpected type found: ${other.getClass.getSimpleName}")
        true
    }
  }

  private val scalaToJavaPrimitiveTypes: Map[String, String] = Map(
    "scala.Unit" -> "void",
    "scala.Boolean" -> "boolean",
    "scala.Char" -> "char",
    "scala.Byte" -> "byte",
    "scala.Short" -> "short",
    "scala.Int" -> "int",
    "scala.Long" -> "long",
    "scala.Float" -> "float",
    "scala.Double" -> "double"
  )

  private[internal] val scalaAliasesToJavaTypes = {
    Map(
      "scala.Predef.String" -> "java.lang.String",
      "scala.Predef.Class" -> "java.lang.Class",
      "scala.Predef.Function" -> "scala.Function1",
      "scala.Predef.Map" -> "scala.collection.immutable.Map",
      "scala.Predef.Set" -> "scala.collection.immutable.Set",
      "scala.Predef.OptManifest" -> "scala.reflect.OptManifest",
      "scala.Predef.Manifest" -> "scala.reflect.Manifest",
      "scala.Predef.Pair" -> "scala.Tuple2",
      "scala.Predef.Triple" -> "scala.Tuple3",
      "scala.package.Throwable" -> "java.lang.Throwable",
      "scala.package.Exception" -> "java.lang.Exception",
      "scala.package.Error" -> "java.lang.Error",
      "scala.package.RuntimeException" -> "java.lang.RuntimeException",
      "scala.package.NullPointerException" -> "java.lang.NullPointerException",
      "scala.package.ClassCastException " -> "java.lang.ClassCastException",
      "scala.package.IndexOutOfBoundsException" -> "java.lang.IndexOutOfBoundsException",
      "scala.package.ArrayIndexOutOfBoundsException" -> "java.lang.ArrayIndexOutOfBoundsException",
      "scala.package.StringIndexOutOfBoundsException" -> "java.lang.StringIndexOutOfBoundsException",
      "scala.package.UnsupportedOperationException" -> "java.lang.UnsupportedOperationException",
      "scala.package.IllegalArgumentException" -> "java.lang.IllegalArgumentException",
      "scala.package.NoSuchElementException" -> "java.util.NoSuchElementException",
      "scala.package.NumberFormatException" -> "java.lang.NumberFormatException",
      "scala.package.AbstractMethodError" -> "java.lang.AbstractMethodError",
      "scala.package.InterruptedException" -> "java.lang.InterruptedException",
      "scala.package.Iterable" -> "scala.collection.Iterable",
      "scala.package.Seq" -> "scala.collection.Seq",
      "scala.package.IndexedSeq" -> "scala.collection.IndexedSeq",
      "scala.package.Iterator" -> "scala.collection.Iterator",
      "scala.package.BufferedIterator" -> "scala.collection.BufferedIterator",
      "scala.package.List" -> "scala.collection.immutable.List",
      "scala.package.::" -> "scala.collection.immutable.$colon$colon",
      "scala.package.Stream" -> "scala.collection.immutable.Stream",
      "scala.package.Vector" -> "scala.collection.immutable.Vector",
      "scala.package.StringBuilder" -> "scala.collection.mutable.StringBuilder",
      "scala.package.Range" -> "scala.collection.immutable.Range",
      "scala.package.BigDecimal" -> "scala.math.BigDecimal",
      "scala.package.BigInt" -> "scala.math.BigInt",
      "scala.package.Equiv" -> "scala.math.Equiv",
      "scala.package.Fractional" -> "scala.math.Fractional",
      "scala.package.Integral" -> "scala.math.Integral",
      "scala.package.Numeric" -> "scala.math.Numeric",
      "scala.package.Ordered" -> "scala.math.Ordered",
      "scala.package.Ordering" -> "scala.math.Ordering",
      "scala.package.PartialOrdering" -> "scala.math.PartialOrdering",
      "scala.package.PartiallyOrdered" -> "scala.math.PartiallyOrdered",
      "scala.package.Either" -> "scala.util.Either",
      "scala.package.Left" -> "scala.util.Left",
      "scala.package.Right" -> "scala.util.Right"
    ) ++ (
      if (scalaVersion.startsWith("2.12"))
        Map(
          "scala.Predef.ClassManifest" -> "scala.reflect.ClassTag",
          "scala.package.TraversableOnce" -> "scala.collection.TraversableOnce",
          "scala.package.Traversable" -> "scala.collection.Traversable"
        )
      else
        Map(
          "scala.package.Cloneable" -> "java.lang.Cloneable",
          "scala.package.Seriazable" -> "java.io.Serializable",
          "scala.package.TraversableOnce" -> "scala.collection.IterableOnce",
          "scala.package.IterableOnce" -> "scala.collection.IterableOnce",
          "scala.package.Traversable" -> "scala.collection.Iterable",
          "scala.package.LazyList" -> "scala.collection.immutable.LazyList"
        )
    )
  }

  private[internal] val scalaToJavaTypes =
    scalaToJavaPrimitiveTypes ++ scalaAliasesToJavaTypes

  private def matchTypeSymbol(
      javaType: jdi.Type,
      sym: Symbol,
      declaringType: jdi.Type
  ): Boolean = {
    val encoded = tryEncodeType(sym)
    val path = sym.path
    sym match {
      case TypeSymbol(_) =>
        // TypeSymbol becomes java.lang.Object after erasure
        javaType.name == "java.lang.Object"
      case AliasSymbol(info) =>
        val tpe = info.info.get
        matchType(javaType, tpe, declaringType)
      case _ =>
        if (path == "scala.Array") {
          javaType.isInstanceOf[jdi.ArrayType]
        } else if (scalaToJavaTypes.contains(path)) {
          // sym is a primitive or a type alias from the Scala library
          javaType.name == scalaToJavaTypes(path)
        } else if (sourceLookUp.containsClass(encoded)) {
          // sym is a real top level class
          javaType.name == encoded
        } else {
          // the symbol is not know by the class loader
          // if it is a type alias, we don't know if it matches the java type
          // so we return true, not to skip a method that should not be skipped
          true
        }
    }
  }

  private def tryEncodeType(sym: Symbol): String = {
    def encodePrefix(sym: Symbol): String = {
      sym match {
        case NoSymbol => ""
        case _: ExternalSymbol =>
          // we weakly assume it is a package
          sym.parent.map(encodePrefix).getOrElse("") + sym.name + "."
        case _ => sym.parent.map(encodePrefix).getOrElse("") + sym.name + "$"
      }
    }
    sym match {
      case obj: ObjectSymbol =>
        obj.parent.map(encodePrefix).getOrElse("") + obj.name + "$"
      case _ => sym.parent.map(encodePrefix).getOrElse("") + sym.name
    }
  }
}
