package ch.epfl.scala.debugadapter.internal.stacktrace

import com.sun.jdi
import scala.jdk.CollectionConverters.*

object JdiExtensions {
  private val lazyTypes: Set[String] = Set(
    "scala.runtime.LazyRef",
    "scala.runtime.LazyBoolean",
    "scala.runtime.LazyByte",
    "scala.runtime.LazyChar",
    "scala.runtime.LazyShort",
    "scala.runtime.LazyInt",
    "scala.runtime.LazyLong",
    "scala.runtime.LazyFloat",
    "scala.runtime.LazyDouble",
    "scala.runtime.LazyUnit"
  )

  implicit class JdiMethodExtension(val method: jdi.Method) extends AnyVal {
    def isStaticMain: Boolean =
      method.isStatic && method.name == "main"

    def isJava: Boolean =
      method.declaringType.sourceName.endsWith(".java")

    def isConstructor: Boolean =
      method.name == "<init>"

    def isStaticConstructor: Boolean =
      method.name == "<clinit>"

    def isAnonFunction: Boolean =
      method.name.matches(".+\\$anonfun\\$\\d+")

    def isLiftedMethod: Boolean =
      method.name.matches(".+\\$\\d+")

    def isAdaptedMethod: Boolean =
      method.name.matches(".+\\$adapted(\\$\\d+)?")

    def isLazyInitializer: Boolean =
      method.name.contains("$lzyINIT") || method.name.contains("$lzycompute")

    def isLazyGetter: Boolean =
      method.argumentTypeNames.asScala.toSeq match {
        case Seq(argTypeName) => lazyTypes.contains(argTypeName)
        case _ => false
      }

    def isDefaultValue: Boolean =
      method.name.contains("$default$")

    def isTraitInitializer: Boolean =
      method.name == "$init$"

    def isPrivateAccessor: Boolean =
      method.name.matches(""".+\$access\$\d+""")
  }

  implicit class JdiReferenceTypeExtension(val tpe: jdi.ReferenceType) extends AnyVal {
    def isDynamicClass: Boolean =
      try
        // source of java.lang.invoke.LambdaForm$DMH.1175962212.invokeStatic_L_L(java.lang.Object, java.lang.Object) is LambdaForm$DMH
        !tpe.sourceName.contains('.')
      catch {
        case _: jdi.AbsentInformationException =>
          // We assume that a ReferenceType with no source name is necessarily a dynamic class
          true
      }

    def isAnonClass: Boolean =
      tpe.name.contains("$anon$")

    /** is local class or local object */
    def isLocalClass: Boolean =
      tpe.name.matches(".+\\$\\d+\\$?")

    def isNestedClass: Boolean =
      tpe.name.matches(".+\\$\\.+")
  }
}
