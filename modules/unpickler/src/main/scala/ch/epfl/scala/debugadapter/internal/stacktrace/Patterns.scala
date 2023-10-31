package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.internal.binary
import scala.util.matching.Regex

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
    def unapply(method: binary.Method): Option[Seq[String]] =
      method match
        case ByNameArgProxy() => None
        case DefaultArg(_) => None
        case _ => method.extractFromDecodedNames("(.+)\\$\\d+".r)(_(0))

  object DefaultArg:
    def unapply(method: binary.Method): Option[Seq[String]] =
      method.extractFromDecodedNames("(.+)\\$default\\$\\d+".r)(_(0))

  object LocalLazyInit:
    def unapply(method: binary.Method): Option[Seq[String]] =
      if !method.allParameters.forall(_.isGenerated) then None
      else method.extractFromDecodedNames("(.+)\\$lzyINIT\\d+\\$(\\d+)".r)(_(0))

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
      val traitSetter = ".+\\$_setter_\\$(.+\\$\\$)?(.+)_=".r
      traitSetter.unapplySeq(method.decodedName).map(xs => xs(1))

  object Setter:
    def unapply(method: binary.Method): Option[Seq[String]] =
      method.extractFromDecodedNames("(.+)_=".r)(_(0))

  object SuperAccessor:
    def unapply(method: binary.Method): Option[Seq[String]] =
      method.extractFromDecodedNames("super\\$(.+)".r)(_(0))

  object SpecializedMethod:
    def unapply(method: binary.Method): Option[Seq[String]] =
      method.extractFromDecodedNames("(.+)\\$mc.+\\$sp".r)(_(0))

  object ByNameArgProxy:
    def unapply(method: binary.Method): Boolean =
      ".+\\$proxy\\d+\\$\\d+".r.unapplySeq(method.name).isDefined

  object InlineAccessor:
    def unapply(method: binary.Method): Option[Seq[String]] =
      method.extractFromDecodedNames("inline\\$(.+)".r)(_(0))

  extension (method: binary.Method)
    private def extractFromDecodedNames[T](regex: Regex)(extract: List[String] => T): Option[Seq[T]] =
      val extracted = method.unexpandedDecodedNames
        .flatMap(regex.unapplySeq)
        .map(extract)
        .distinct
      if extracted.nonEmpty then Some(extracted) else None
