package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.internal.binary

object Patterns:
  object LocalClass:
    def unapply(cls: binary.ClassType): Option[(String, String, Option[String])] =
      val decodedClassName = NameTransformer.decode(cls.name.split('.').last)
      unapply(decodedClassName)

    def unapply(decodedClassName: String): Option[(String, String, Option[String])] =
      "(.+)\\$([^$]+)\\$\\d+(\\$.*)?".r
        .unapplySeq(decodedClassName)
        .filter(xs => xs(1) != "anon")
        .map(xs => (xs(0), xs(1), Option(xs(2)).map(_.stripPrefix("$")).filter(_.nonEmpty)))

  object AnonClass:
    def unapply(cls: binary.ClassType): Option[(String, Option[String])] =
      val decodedClassName = NameTransformer.decode(cls.name.split('.').last)
      unapply(decodedClassName)

    def unapply(decodedClassName: String): Option[(String, Option[String])] =
      "(.+)\\$\\$anon\\$\\d+(\\$.*)?".r
        .unapplySeq(decodedClassName)
        .map(xs => (xs(0), Option(xs(1)).map(_.stripPrefix("$")).filter(_.nonEmpty)))

  object InnerClass:
    def unapply(cls: binary.ClassType): Option[String] =
      val decodedClassName = NameTransformer.decode(cls.name.split('.').last)
      "(.+)\\$(.+)".r
        .unapplySeq(decodedClassName)
        .map(_ => decodedClassName)

  object LazyInit:
    def unapply(method: binary.Method): Option[String] =
      val lazyInit = "(.*)\\$lzyINIT\\d+".r
      lazyInit.unapplySeq(NameTransformer.decode(method.name)).map(xs => xs(0))

  object TraitStaticForwarder:
    def unapply(method: binary.Method): Option[String] =
      if method.isTraitStaticForwarder then Some(method.name.stripSuffix("$"))
      else None

  object Outer:
    def unapply(method: binary.Method): Option[String] =
      val anonFun = "(.*)\\$\\$\\$outer".r
      anonFun.unapplySeq(NameTransformer.decode(method.name)).map(xs => xs(0))

  object AnonFun:
    def unapply(method: binary.Method): Option[String] =
      val anonFun = "(.*)\\$anonfun\\$\\d+".r
      anonFun.unapplySeq(NameTransformer.decode(method.name)).map(xs => xs(0).stripSuffix("$"))

  object AdaptedAnonFun:
    def unapply(method: binary.Method): Option[String] =
      val adaptedAnonFun = "(.*)\\$anonfun\\$adapted\\$\\d+".r
      adaptedAnonFun.unapplySeq(NameTransformer.decode(method.name)).map(xs => xs(0).stripSuffix("$"))

  object LocalMethod:
    def unapply(method: binary.Method): Option[(String, Int)] =
      if method.name.contains("$default") || method.name.contains("$proxy") then None
      else
        "(.+)\\$(\\d+)".r
          .unapplySeq(method.unexpandedDecodedName)
          .map(xs => (xs(0), xs(1).toInt))

  object LocalLazyInit:
    def unapply(method: binary.Method): Option[(String, Int)] =
      if !method.allParameters.forall(_.isGenerated) then None
      else
        "(.+)\\$lzyINIT\\d+\\$(\\d+)".r
          .unapplySeq(method.unexpandedDecodedName)
          .map(xs => (xs(0), xs(1).toInt))

  object SuperArg:
    def unapply(method: binary.Method): Boolean =
      val superArg = "(.*)\\$superArg\\$\\d+(\\$\\d+)?".r
      superArg.unapplySeq(method.name).isDefined

  object LiftedTree:
    def unapply(method: binary.Method): Boolean =
      val liftedTree = "liftedTree\\d+\\$\\d+".r
      liftedTree.unapplySeq(method.name).isDefined

  object TraitInitializer:
    def unapply(method: binary.Method): Boolean =
      method.isTraitInitializer

  object ValueClassExtension:
    def unapply(method: binary.Method): Boolean =
      method.isExtensionMethod

  object DeserializeLambda:
    def unapply(method: binary.Method): Boolean =
      method.isDeserializeLambda

  object ParamForwarder:
    def unapply(method: binary.Method): Option[String] =
      val paramForwarder = "(.+)\\$accessor".r
      paramForwarder.unapplySeq(method.name).map(xs => xs(0))

  object TraitSetter:
    def unapply(method: binary.Method): Option[String] =
      val traitSetter = ".+\\$_setter_\\$.+\\$\\$(.+)_=".r
      traitSetter.unapplySeq(method.decodedName).map(xs => xs(0))

  object SuperAccessor:
    def unapply(method: binary.Method): Option[String] =
      val superAccessor = "super\\$(.+)".r
      superAccessor.unapplySeq(method.unexpandedDecodedName).map(xs => xs(0))
