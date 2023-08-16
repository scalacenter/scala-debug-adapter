package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.internal.binary

object LocalClass:
  def unapply(cls: binary.ClassType): Option[(String, String, Option[String])] =
    val decodedClassName = NameTransformer.decode(cls.name.split('.').last)
    unapply(decodedClassName)

  def unapply(decodedClassName: String): Option[(String, String, Option[String])] =
    "(.+)\\$([^$]+)\\$\\d+(\\$.*)?".r
      .unapplySeq(NameTransformer.decode(decodedClassName))
      .filter(xs => xs(1) != "anon")
      .map(xs => (xs(0), xs(1), Option(xs(2)).map(_.stripPrefix("$")).filter(_.nonEmpty)))

object AnonClass:
  def unapply(cls: binary.ClassType): Option[(String, Option[String])] =
    val decodedClassName = NameTransformer.decode(cls.name.split('.').last)
    unapply(decodedClassName)
  def unapply(decodedClassName: String): Option[(String, Option[String])] =
    "(.+)\\$\\$anon\\$\\d+(\\$.*)?".r
      .unapplySeq(NameTransformer.decode(decodedClassName))
      .map(xs => (xs(0), Option(xs(1)).map(_.stripPrefix("$")).filter(_.nonEmpty)))

object InnerClass:
  def unapply(cls: binary.ClassType): Option[String] =
    val decodedClassName = NameTransformer.decode(cls.name.split('.').last)
    unapply(decodedClassName)

  def unapply(decodedClassName: String): Option[String] =
    "(.+)\\$(.+)".r
      .unapplySeq(NameTransformer.decode(decodedClassName))
      .map(_ => decodedClassName)

object LazyInit:
  def unapply(method: binary.Method): Option[String] =
    val lazyInit = "(.*)\\$lzyINIT\\d+".r
    lazyInit.unapplySeq(NameTransformer.decode(method.name)).map(xs => xs(0))

object StaticAccessor:
  def unapply(method: binary.Method): Option[String] =
    if method.isTraitStaticAccessor then Some(method.name.stripSuffix("$"))
    else None

object AnonFun:
  def unapply(method: binary.Method): Option[String] =
    val anonFun = "(.*)\\$anonfun\\$\\d+".r
    anonFun.unapplySeq(NameTransformer.decode(method.name)).map(xs => xs(0).stripSuffix("$"))

object LocalMethod:
  def unapply(method: binary.Method): Option[(String, Int)] =
    if method.name.contains("$default") || method.name.contains("$proxy") then None
    else
      val javaPrefix = method.declaringClass.name.replace('.', '$') + "$$"
      val decodedName = NameTransformer.decode(method.name.stripPrefix(javaPrefix).split("_\\$").last)
      "(.+)\\$(\\d+)".r.unapplySeq(decodedName).map(xs => (xs(0), xs(1).toInt))

object LocalLazyInit:
  def unapply(method: binary.Method): Option[(String, Int)] =
    if method.declaredParams.nonEmpty then None
    else
      val javaPrefix = method.declaringClass.name.replace('.', '$') + "$$"
      val decodedName = NameTransformer.decode(method.name.stripPrefix(javaPrefix).split("\\$_\\$").last)
      "(.+)\\$lzyINIT\\d+\\$(\\d+)".r.unapplySeq(decodedName).map(xs => (xs(0), xs(1).toInt))
