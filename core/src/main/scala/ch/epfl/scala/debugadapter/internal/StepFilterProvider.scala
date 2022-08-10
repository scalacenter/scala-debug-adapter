package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.internal.scalasig._
import com.microsoft.java.debug.core.adapter.{
  StepFilterProvider => JavaStepFilterProvider
}
import com.microsoft.java.debug.core.protocol.Requests.StepFilters
import com.sun.jdi.LocalVariable
import com.sun.jdi.Method
import com.sun.jdi.ReferenceType
import com.sun.jdi.ClassType
import com.sun.jdi.InterfaceType
import com.sun.jdi.AbsentInformationException

import scala.collection.JavaConverters._
import ch.epfl.scala.debugadapter.Logger

class StepFilterProvider(sourceLookUp: SourceLookUpProvider, logger: Logger)
    extends JavaStepFilterProvider() {

  override def skip(method: Method, filters: StepFilters): Boolean = {
    try {
      if (method.isBridge) true
      else if (isDynamicClass(method.declaringType)) true
      else if (super.skip(method, filters)) true
      else if (isJava(method)) false
      else if (isLocalMethod(method)) false
      else if (isLocalClass(method.declaringType)) false
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

  private def isDynamicClass(tpe: ReferenceType): Boolean =
    try {
      tpe.sourceName()
      false
    } catch {
      case _: AbsentInformationException =>
        // We assume that a ReferenceType with no source name is necessarily a dynamic class
        true
    }

  private def isJava(method: Method): Boolean =
    method.declaringType.sourceName.endsWith(".java")

  private def isLocalMethod(method: Method): Boolean =
    method.name().contains("$anonfun$")

  private def isLocalClass(tpe: ReferenceType): Boolean =
    tpe.name().contains("$anon$")

  private def isLazyInitializer(method: Method): Boolean =
    method.name.endsWith("$lzycompute")

  private def skipLazyInitializer(method: Method): Boolean = {
    val fieldName = method.name.dropRight(11)
    method.declaringType match {
      case cls: ClassType =>
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

  private def skipScalaMethod(method: Method): Boolean = {
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
      val formattedMethods =
        matchingMethods.map(s => s"$s ${s.info.flags}: ${s.infoType}")
      throw new Exception(
        s"Found ${matchingMethods.size} matching symbols: " + formattedMethods
          .mkString(", ")
      )
    }

    val res = matchingMethods.headOption.forall(skip)

    if (res) {
      logger.debug(s"Skipping $method")
    }

    res
  }

  private def containsLazyField(
      interface: InterfaceType,
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
      javaMethod: Method,
      scalaMethod: MethodSymbol
  ): Boolean = {
    if (scalaMethod.aliasRef.nonEmpty)
      logger.debug(
        s"aliasRef for ${scalaMethod.name}: ${scalaMethod.aliasRef}"
      )
    if (scalaMethod.isSyntheticMethod)
      logger.debug(s"${scalaMethod.name} isSyntheticMethod")
    if (scalaMethod.isMonomorphic)
      logger.debug(s"${scalaMethod.name} isMonomorphic")

    javaMethod.name == scalaMethod.name &&
    matchArguments(javaMethod, scalaMethod.infoType) &&
    matchOwner(javaMethod.declaringType(), scalaMethod.parent.get)
  }

  private def matchOwner(
      javaClass: ReferenceType,
      scalaOwner: Symbol
  ): Boolean = {
    val fqcn = javaClass.name()
    // TODO improve
    getOwners(scalaOwner).reverse
      .foldLeft(Option(fqcn)) { (acc, sym) =>
        for (fqcn <- acc if fqcn.contains(sym.name)) yield {
          fqcn
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

  private def matchArguments(
      javaMethod: Method,
      methodType: scalasig.Type
  ): Boolean = {
    val javaArgs = javaMethod.arguments().asScala.toSeq
    val scalaArgs = extractParameters(methodType)
    javaArgs.size == scalaArgs.size &&
    javaArgs.zip(scalaArgs).forall { case (javaArg, scalaArg) =>
      matchArgument(javaArg, scalaArg)
    }
  }

  private def matchArgument(
      javaArg: LocalVariable,
      scalaArg: Symbol
  ): Boolean = {
    javaArg.name() == scalaArg.name
  }

  private def extractParameters(methodType: scalasig.Type): Seq[Symbol] = {
    methodType match {
      case m: FunctionType =>
        m.paramSymbols ++ extractParameters(m.resultType.get)
      case m: NullaryMethodType => Seq.empty
      case m: PolyType => extractParameters(m.typeRef.get)
      case _: TypeRefType => Seq.empty
      case other =>
        val className = other.getClass.getSimpleName()
        throw new Exception(s"unexpected type found: $className")
    }
  }
}
