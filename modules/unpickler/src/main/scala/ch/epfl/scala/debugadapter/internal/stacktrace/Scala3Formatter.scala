package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Types.*
import tastyquery.Names.*
import ch.epfl.scala.debugadapter.internal.stacktrace.BinaryMethodKind.*
import ch.epfl.scala.debugadapter.internal.stacktrace.*
import ch.epfl.scala.debugadapter.internal.stacktrace.BinaryMethodSymbol.*

class Scala3Formatter(warnLogger: String => Unit, testMode: Boolean) extends ThrowOrWarn(warnLogger, testMode):
  def format(binaryClass: BinaryClassSymbol): String =
    binaryClass match
      case BinaryClass(symbol) => formatSymbol(symbol)
      case BinarySAMClass(symbol, _, _) => formatOwner(symbol.owner) + ".<SAM class>"
      case BinaryPartialFunction(symbol, _) => formatOwner(symbol.owner) + ".<partial function>"
      case BinarySyntheticCompanionClass(symbol) => formatSymbol(symbol)

  def format(method: BinaryMethodSymbol): String =
    def formatSym(method: BinaryMethodSymbol): String =
      method match
        case BinaryMethod(owner, term, kind) =>
          kind match
            case BinaryMethodKind.AdaptedAnonFun => formatSymbol(term) + ".<adapted>"
            case BinaryMethodKind.Setter => formatSymbol(term)
            case BinaryMethodKind.LazyInit => formatSymbol(owner.symbol, term.name) + ".<lazy init>"
            case BinaryMethodKind.LocalLazyInit => formatSymbol(term) + ".<lazy init>"
            case BinaryMethodKind.TraitStaticForwarder => formatSymbol(term) + ".<static forwarder>"
            case BinaryMethodKind.TraitParamGetter => formatSymbol(owner.symbol, term.name)
            case BinaryMethodKind.TraitParamSetter => formatSymbol(owner.symbol, term.name)
            case BinaryMethodKind.MixinForwarder => formatSymbol(owner.symbol, term.name) + ".<mixin forwarder>"
            case _ => formatSymbol(term)
        case BinaryOuter(owner, _) => format(owner) + ".<outer>"
        case BinarySuperArg(_, init, _) => formatSymbol(init) + ".<super arg>"
        case BinaryLiftedTry(owner, _) => format(owner) + ".<try>"
        case BinaryByNameArg(owner, _, adapted) =>
          format(owner) + ".<by-name arg>" + (if adapted then ".<adapted>" else "")
        case BinaryMethodBridge(target, _) => formatSym(target) + ".<bridge>"
        case BinaryAnonOverride(owner, overridden, _) => format(owner) + "." + formatName(overridden.name)
        case BinaryStaticForwarder(owner, target) =>
          formatSymbol(owner.symbol, target.term.name) + ".<static forwarder>"
    val typeAscription: String =
      method match
        case BinaryMethod(_, term, _) => formatTypeAndSep(term.declaredType)
        case BinaryOuter(_, outer) => ": " + formatOwner(outer) // TODO fix, get the type
        case BinarySuperArg(_, _, tpe) => formatTypeAndSep(tpe)
        case BinaryLiftedTry(_, tpe) => formatTypeAndSep(tpe)
        case BinaryByNameArg(_, tpe, _) => formatTypeAndSep(tpe)
        case BinaryMethodBridge(_, tpe) => formatTypeAndSep(tpe)
        case BinaryAnonOverride(_, _, tpe) => formatTypeAndSep(tpe)
        case BinaryStaticForwarder(_, target) => formatTypeAndSep(target.term.declaredType)
    formatSym(method) + typeAscription

  private def formatSymbol(sym: Symbol): String =
    formatSymbol(sym.owner, sym.name)

  private def formatSymbol(owner: Symbol, name: Name): String =
    val prefix = formatOwner(owner)
    val nameStr = formatName(name)
    if prefix.isEmpty then nameStr else s"$prefix.$nameStr"

  private def formatOwner(sym: Symbol): String =
    sym match
      case sym: ClassSymbol if sym.name.isPackageObject => formatSymbol(sym.owner)
      case sym: TermOrTypeSymbol => formatSymbol(sym)
      case sym: PackageSymbol => ""

  private def formatName(name: Name): String =
    def rec(name: Name): String = name match
      case DefaultGetterName(termName, num) => s"${termName.toString()}.<default ${num + 1}>"
      case TypeName(toTermName) => rec(toTermName)
      case SimpleName("$anonfun") => "<anon fun>"
      case SimpleName("$anon") => "<anon class>"
      case _ => name.toString
    rec(name)

  private def formatTypeAndSep(t: TermType): String = t match
    case _: MethodicType => formatType(t)
    case _ => ": " + formatType(t)

  private def formatType(t: TermType | TypeOrWildcard): String =
    t match
      case t: MethodType =>
        val params = t.paramNames
          .map(paramName =>
            val pattern = """.+\$\d+$""".r
            if pattern.matches(paramName.toString) then ""
            else s"$paramName: "
          )
          .zip(t.paramTypes)
          .map((n, t) => s"$n${formatType(t)}")
          .mkString(", ")
        val sep = if t.resultType.isInstanceOf[MethodicType] then "" else ": "
        val result = formatType(t.resultType)
        val prefix =
          if t.isContextual then "using "
          else if t.isImplicit then "implicit "
          else ""
        s"($prefix$params)$sep$result"
      case t: TypeRef => formatPrefix(t.prefix) + t.name
      case t: AppliedType if t.tycon.isFunction =>
        val args = t.args.init.map(formatType).mkString(", ")
        val result = formatType(t.args.last)
        t.args.size match
          case 2 => s"$args => $result"
          case _ => s"($args) => $result"
      case t: AppliedType if t.tycon.isContextFunction =>
        val args = t.args.init.map(formatType).mkString(", ")
        val result = formatType(t.args.last)
        t.args.size match
          case 2 => s"$args ?=> $result"
          case _ => s"($args) ?=> $result"
      case t: AppliedType if t.tycon.isTuple =>
        val types = t.args.map(formatType).mkString(", ")
        s"($types)"
      case t: AppliedType if t.tycon.isOperatorLike && t.args.size == 2 =>
        val operatorLikeTypeFormat = t.args
          .map(formatType)
          .mkString(" " + t.tycon.asInstanceOf[TypeRef].name.toString + " ")
        operatorLikeTypeFormat
      case t: AppliedType if t.tycon.isVarArg =>
        s"${formatType(t.args.head)}*"
      case t: AppliedType =>
        val tycon = formatType(t.tycon)
        val args = t.args.map(formatType).mkString(", ")
        s"$tycon[$args]"
      case t: PolyType =>
        val args = t.paramNames.mkString(", ")
        val sep = if t.resultType.isInstanceOf[MethodicType] then "" else ": "
        val result = formatType(t.resultType)
        s"[$args]$sep$result"
      case t: OrType =>
        val first = formatType(t.first)
        val second = formatType(t.second)
        s"$first | $second"
      case t: AndType =>
        val first = formatType(t.first)
        val second = formatType(t.second)
        s"$first & $second"
      case t: ThisType => formatType(t.tref)
      case t: TermRefinement =>
        val parentType = formatType(t.parent)
        if parentType == "PolyFunction" then formatPolymorphicFunction(t.refinedType)
        else parentType + " {...}"
      case t: AnnotatedType => formatType(t.typ)
      case t: TypeParamRef => t.paramName.toString
      case t: TermParamRef => formatPrefix(t) + "type"
      case t: TermRef => formatPrefix(t) + "type"
      case t: ConstantType =>
        t.value.value match
          case str: String => s"\"$str\""
          case t: Type =>
            // to reproduce this we should try `val x = classOf[A]`
            s"classOf[${formatType(t)}]"
          case v => v.toString
      case t: ByNameType => s"=> " + formatType(t.resultType)
      case t: TypeRefinement => formatType(t.parent) + " {...}"
      case t: RecType => formatType(t.parent)
      case _: WildcardTypeArg => "?"
      case t: TypeLambda =>
        val args = t.paramNames.map(t => t.toString).mkString(", ")
        val result = formatType(t.resultType)
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
        val params = t.paramTypes.map(formatType(_)).mkString(", ")
        if t.paramTypes.size > 1 then s"($params) => ${formatType(t.resultType)}"
        else s"$params => ${formatType(t.resultType)}"

  private def formatPrefix(p: Prefix): String =
    val prefix = p match
      case NoPrefix => ""
      case p: TermRef if isScalaPredef(p) => ""
      case p: TermRef if isPackageObject(p.name) => ""
      case p: TermRef => formatPrefix(p.prefix) + p.name
      case p: TermParamRef => p.paramName.toString
      case p: PackageRef => ""
      case p: ThisType => ""
      case t: Type => formatType(t)

    if prefix.nonEmpty then s"$prefix." else prefix
