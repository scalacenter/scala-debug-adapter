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
      case BinarySAMClass(symbol, _, _) => formatQualifiedName(symbol.owner) + ".<SAM class>"
      case BinaryPartialFunction(symbol, _) => formatQualifiedName(symbol.owner) + ".<partial function>"
      case BinarySyntheticCompanionClass(symbol) => formatQualifiedName(symbol)

  def format(method: BinaryMethodSymbol): String =
    formatQualifiedName(method) + formatTypeAscription(method)

  private def formatQualifiedName(method: BinaryMethodSymbol): String =
    method match
      case BinaryMethod(_, sym) => formatQualifiedName(sym, "")
      case BinaryAnonFun(_, sym, adapted) => formatQualifiedName(sym, if adapted then "<adapted>" else "")
      case BinaryLocalLazyInit(_, sym) => formatQualifiedName(sym, "<lazy init>")
      case BinaryLazyInit(owner, sym) => formatQualifiedName(owner, sym, "<lazy init>")
      case BinaryTraitParamAccessor(owner, sym) => formatQualifiedName(owner, sym, "")
      case BinaryMixinForwarder(owner, sym) => formatQualifiedName(owner, sym, "<mixin forwarder>")
      case BinaryTraitStaticForwarder(_, sym) => formatQualifiedName(sym, "<static forwarder>")
      case BinaryOuter(owner, _) => format(owner) + ".<outer>"
      case BinarySuperArg(_, init, _) => formatQualifiedName(init, "<super arg>")
      case BinaryLiftedTry(owner, _) => formatQualifiedName(owner, "<try>")
      case BinaryByNameArg(owner, _, adapted) =>
        formatQualifiedName(owner, "<by-name arg>" + (if adapted then ".<adapted>" else ""))
      case BinaryMethodBridge(owner, target, _) => formatQualifiedName(owner, target, "<bridge>")
      case BinaryAnonOverride(owner, overridden, _) => formatQualifiedName(owner, overridden, "")
      case BinaryStaticForwarder(owner, target, _) => formatQualifiedName(owner, target, "<static forwarder>")
      case BinaryDeserializeLambda(owner) => format(owner) + ".$deserializeLambda$"
      case BinarySetter(owner, sym, _) =>
        if sym.isMethod then formatQualifiedName(sym, "")
        else format(owner) + "." + format(sym.name) + "_="
      case BinarySuperAccessor(owner, sym, _, isBridge) =>
        formatQualifiedName(owner, sym, if isBridge then "<super>.<bridge>" else "<super>")
      case BinarySpecializedMethod(_, sym) => formatQualifiedName(sym, "<specialized>")
      case BinaryInlineAccessor(target) => formatQualifiedName(target) + ".<inline>"

  private def formatTypeAscription(method: BinaryMethodSymbol): String =
    method match
      case BinaryMethod(_, sym) => formatTypeAscription(sym.declaredType)
      case BinaryAnonFun(_, sym, _) => formatTypeAscription(sym.declaredType)
      case BinaryLocalLazyInit(_, sym) => formatTypeAscription(sym.declaredType)
      case BinaryLazyInit(_, sym) => formatTypeAscription(sym.declaredType)
      case BinaryTraitParamAccessor(_, sym) => formatTypeAscription(sym.declaredType)
      case BinaryMixinForwarder(_, sym) => formatTypeAscription(sym.declaredType)
      case BinaryTraitStaticForwarder(_, sym) => formatTypeAscription(sym.declaredType)
      case BinaryOuter(_, tpe) => formatTypeAscription(tpe)
      case BinarySuperArg(_, _, tpe) => formatTypeAscription(tpe)
      case BinaryLiftedTry(_, tpe) => formatTypeAscription(tpe)
      case BinaryByNameArg(_, tpe, _) => formatTypeAscription(tpe)
      case BinaryMethodBridge(_, _, tpe) => formatTypeAscription(tpe)
      case BinaryAnonOverride(_, _, tpe) => formatTypeAscription(tpe)
      case BinaryStaticForwarder(_, _, tpe) => formatTypeAscription(tpe)
      case _: BinaryDeserializeLambda => "(SerializedLambda): Object"
      case BinarySetter(_, _, paramType) => "(" + format(paramType) + "): Unit"
      case BinarySuperAccessor(_, _, tpe, _) => formatTypeAscription(tpe)
      case BinarySpecializedMethod(_, sym) => formatTypeAscription(sym.declaredType)
      case BinaryInlineAccessor(target) => formatTypeAscription(target)

  private def formatQualifiedName(term: TermSymbol, suffix: String): String =
    val suffixSep = if suffix.isEmpty then "" else "."
    formatQualifiedName(term) + suffixSep + suffix

  private def formatQualifiedName(owner: BinaryClassSymbol, term: TermSymbol, suffix: String): String =
    val suffixSep = if suffix.isEmpty then "" else "."
    format(owner) + "." + format(term.name) + suffixSep + suffix

  private def formatQualifiedName(owner: BinaryClassSymbol, suffix: String): String =
    val suffixSep = if suffix.isEmpty then "" else "."
    format(owner) + suffixSep + suffix

  private def formatQualifiedName(sym: Symbol): String =
    sym match
      case sym: ClassSymbol if sym.name.isPackageObject => formatQualifiedName(sym.owner)
      case sym =>
        val prefix = sym.owner match
          case sym: ClassSymbol if sym.name.isPackageObject => formatQualifiedName(sym.owner)
          case sym: TermOrTypeSymbol => formatQualifiedName(sym)
          case sym: PackageSymbol => ""
        val nameStr = format(sym.name)
        if prefix.isEmpty then nameStr else s"$prefix.$nameStr"

  private def formatTypeAscription(tpe: TypeOrMethodic): String =
    tpe match
      case tpe: Type => ": " + format(tpe)
      case tpe => format(tpe)

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
