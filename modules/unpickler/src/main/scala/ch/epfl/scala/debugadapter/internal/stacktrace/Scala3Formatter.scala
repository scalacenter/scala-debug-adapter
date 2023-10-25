package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Types.*
import tastyquery.Names.*
import ch.epfl.scala.debugadapter.internal.stacktrace.*
import tastyquery.Contexts.Context

class Scala3Formatter(warnLogger: String => Unit, testMode: Boolean)(using Context)
    extends ThrowOrWarn(warnLogger, testMode):
  def format(binaryClass: BinaryClassSymbol): String =
    binaryClass match
      case BinaryClass(symbol) => formatQualifiedName(symbol)
      case BinarySAMClass(symbol, _, _) => formatQualifiedName(symbol.owner).dot("<SAM class>")
      case BinaryPartialFunction(symbol, _) => formatQualifiedName(symbol.owner).dot("<partial function>")
      case BinarySyntheticCompanionClass(symbol) => formatQualifiedName(symbol)

  def format(method: BinaryMethodSymbol): String =
    val typeAscription = method.declaredType match
      case tpe: Type => ": " + format(tpe)
      case tpe => format(tpe)
    formatOwner(method).dot(formatName(method)) + typeAscription

  private def formatOwner(method: BinaryMethodSymbol): String =
    method match
      case BinaryMethod(_, sym) => formatOwner(sym)
      case BinaryLocalLazyInit(_, sym) => formatOwner(sym)
      case BinaryLazyInit(owner, _) => format(owner)
      case BinaryTraitParamAccessor(owner, _) => format(owner)
      case BinaryMixinForwarder(owner, _) => format(owner)
      case BinaryTraitStaticForwarder(_, sym) => formatOwner(sym)
      case BinaryOuter(owner, _) => format(owner)
      case BinarySuperArg(_, init, _) => formatOwner(init)
      case BinaryLiftedTry(owner, _) => format(owner)
      case BinaryByNameArg(owner, _) => format(owner)
      case BinaryMethodBridge(target, _) => formatOwner(target)
      case BinaryAnonOverride(owner, _, _) => format(owner)
      case BinaryStaticForwarder(owner, _, _) => format(owner)
      case BinaryDeserializeLambda(owner, _) => format(owner)
      case BinarySetter(owner, _, _) => format(owner)
      case BinarySuperAccessor(owner, _, _) => format(owner)
      case BinarySpecializedMethod(_, sym) => formatOwner(sym)
      case BinaryInlineAccessor(target) => formatOwner(target)
      case BinaryAdaptedFun(target) => formatOwner(target)

  private def formatName(method: BinaryMethodSymbol): String =
    method match
      case BinaryMethod(_, sym) => formatName(sym)
      case BinaryLocalLazyInit(_, sym) => formatName(sym).dot("<lazy init>")
      case BinaryLazyInit(_, sym) => formatName(sym).dot("<lazy init>")
      case BinaryTraitParamAccessor(_, sym) => formatName(sym)
      case BinaryMixinForwarder(_, sym) => formatName(sym).dot("<mixin forwarder>")
      case BinaryTraitStaticForwarder(_, sym) => formatName(sym).dot("<static forwarder>")
      case _: BinaryOuter => "<outer>"
      case BinarySuperArg(_, init, _) => formatName(init).dot("<super arg>")
      case BinaryLiftedTry(_, _) => "<try>"
      case _: BinaryByNameArg => "<by-name arg>"
      case BinaryMethodBridge(target, _) => formatName(target).dot("<bridge>")
      case BinaryAnonOverride(_, overridden, _) => formatName(overridden)
      case BinaryStaticForwarder(_, target, _) => formatName(target).dot("<static forwarder>")
      case _: BinaryDeserializeLambda => "$deserializeLambda$"
      case BinarySetter(_, sym, _) => if sym.isMethod then formatName(sym) else formatName(sym) + "_="
      case BinarySuperAccessor(_, sym, _) => formatName(sym).dot("<super>")
      case BinarySpecializedMethod(_, sym) => formatName(sym).dot("<specialized>")
      case BinaryInlineAccessor(target) => formatName(target).dot("<inline>")
      case BinaryAdaptedFun(target) => formatName(target).dot("<adapted>")

  private def formatQualifiedName(sym: Symbol): String =
    formatOwner(sym).dot(formatName(sym))

  private def formatOwner(sym: Symbol): String =
    sym.owner match
      case owner: ClassSymbol if owner.name.isPackageObject => format(owner.owner.name)
      case owner: TermOrTypeSymbol => formatOwner(owner).dot(formatName(owner))
      case owner: PackageSymbol => ""

  private def formatName(sym: Symbol): String =
    sym match
      case sym: ClassSymbol if sym.name.isPackageObject => format(sym.owner.name)
      case _ => format(sym.name)

  extension (prefix: String)
    def dot(suffix: String): String =
      if prefix.nonEmpty && suffix.nonEmpty then s"$prefix.$suffix" else prefix + suffix

  private def format(name: Name): String =
    def rec(name: Name): String = name match
      case DefaultGetterName(termName, num) => s"${termName.toString()}.<default ${num + 1}>"
      case TypeName(toTermName) => rec(toTermName)
      case SimpleName("$anonfun") => "<anon fun>"
      case SimpleName("$anon") => "<anon class>"
      case _ => name.toString
    rec(name)

  private def format(t: TermType | TypeOrWildcard): String =
    t match
      case t: MethodType =>
        val params = t.paramNames
          .map(paramName =>
            val pattern = """.+\$\d+$""".r
            if pattern.matches(paramName.toString) then ""
            else s"$paramName: "
          )
          .zip(t.paramTypes)
          .map((n, t) => s"$n${format(t)}")
          .mkString(", ")
        val sep = if t.resultType.isInstanceOf[MethodicType] then "" else ": "
        val result = format(t.resultType)
        val prefix =
          if t.isContextual then "using "
          else if t.isImplicit then "implicit "
          else ""
        s"($prefix$params)$sep$result"
      case t: TypeRef => formatPrefix(t.prefix) + t.name
      case t: AppliedType if t.tycon.isFunction =>
        val args = t.args.init.map(format).mkString(", ")
        val result = format(t.args.last)
        t.args.size match
          case 2 => s"$args => $result"
          case _ => s"($args) => $result"
      case t: AppliedType if t.tycon.isContextFunction =>
        val args = t.args.init.map(format).mkString(", ")
        val result = format(t.args.last)
        t.args.size match
          case 2 => s"$args ?=> $result"
          case _ => s"($args) ?=> $result"
      case t: AppliedType if t.tycon.isTuple =>
        val types = t.args.map(format).mkString(", ")
        s"($types)"
      case t: AppliedType if t.tycon.isOperatorLike && t.args.size == 2 =>
        val operatorLikeTypeFormat = t.args
          .map(format)
          .mkString(" " + t.tycon.asInstanceOf[TypeRef].name.toString + " ")
        operatorLikeTypeFormat
      case t: AppliedType if t.tycon.isRepeatedParam =>
        s"${format(t.args.head)}*"
      case t: AppliedType =>
        val tycon = format(t.tycon)
        val args = t.args.map(format).mkString(", ")
        s"$tycon[$args]"
      case t: PolyType =>
        val args = t.paramNames.mkString(", ")
        val sep = if t.resultType.isInstanceOf[MethodicType] then "" else ": "
        val result = format(t.resultType)
        s"[$args]$sep$result"
      case t: OrType =>
        val first = format(t.first)
        val second = format(t.second)
        s"$first | $second"
      case t: AndType =>
        val first = format(t.first)
        val second = format(t.second)
        s"$first & $second"
      case t: ThisType => format(t.tref) + ".this.type"
      case t: TermRefinement =>
        val parentType = format(t.parent)
        if parentType == "PolyFunction" then formatPolymorphicFunction(t.refinedType)
        else parentType + " {...}"
      case t: AnnotatedType => format(t.typ)
      case t: TypeParamRef => t.paramName.toString
      case t: TermParamRef => formatPrefix(t) + "type"
      case t: TermRef => formatPrefix(t) + "type"
      case t: ConstantType =>
        t.value.value match
          case str: String => s"\"$str\""
          case t: Type =>
            // to reproduce this we should try `val x = classOf[A]`
            s"classOf[${format(t)}]"
          case v => v.toString
      case t: ByNameType => s"=> " + format(t.resultType)
      case t: TypeRefinement => format(t.parent) + " {...}"
      case t: RecType => format(t.parent)
      case _: WildcardTypeArg => "?"
      case t: TypeLambda =>
        val args = t.paramNames.map(t => t.toString).mkString(", ")
        val result = format(t.resultType)
        s"[$args] =>> $result"
      case t @ (_: RecThis | _: SkolemType | _: SuperType | _: MatchType | _: CustomTransientGroundType |
          _: PackageRef) =>
        throwOrWarn(s"Cannot format type ${t.getClass.getName}")
        "<unsupported>"

  private def formatPolymorphicFunction(t: TermType): String =
    t match
      case t: PolyType =>
        val args = t.paramNames.mkString(", ")
        val result = formatPolymorphicFunction(t.resultType)
        s"[$args] => $result"
      case t: MethodType =>
        val params = t.paramTypes.map(format(_)).mkString(", ")
        if t.paramTypes.size > 1 then s"($params) => ${format(t.resultType)}"
        else s"$params => ${format(t.resultType)}"

  private def formatPrefix(p: Prefix): String =
    val prefix = p match
      case NoPrefix => ""
      case p: TermRef if isScalaPredef(p) => ""
      case p: TermRef if isPackageObject(p.name) => ""
      case p: TermRef => formatPrefix(p.prefix) + p.name
      case p: TermParamRef => p.paramName.toString
      case p: PackageRef => ""
      case p: ThisType => ""
      case t: Type => format(t)

    if prefix.nonEmpty then s"$prefix." else prefix
