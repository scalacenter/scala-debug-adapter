package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Types.*
import tastyquery.Names.*
import ch.epfl.scala.debugadapter.internal.stacktrace.BinaryMethodKind.*
import ch.epfl.scala.debugadapter.internal.stacktrace.BinaryClassKind.*
import ch.epfl.scala.debugadapter.internal.stacktrace.BinaryClassSymbol.*
import ch.epfl.scala.debugadapter.internal.stacktrace.BinaryMethodSymbol.*

class Scala3Formatter(warnLogger: String => Unit, testMode: Boolean) extends ThrowOrWarn(warnLogger, testMode):
  private def formatSymbolWithType(symbol: TermSymbol): String =
    val sep = if !symbol.declaredType.isInstanceOf[MethodicType] then ": " else ""
    s"${formatSymbol(symbol)}$sep${formatType(symbol.declaredType)}"

  def format(binaryClass: BinaryClassSymbol) =
    binaryClass match
      case BinaryClass(symbol, Anon) =>
        val prefix = formatOwner(symbol.owner)
        s"$prefix.<anon class>"
      case BinaryClass(symbol, _) => formatSymbol(symbol)
      case BinarySAMClass(symbol, _, samType) =>
        val prefix = formatOwner(symbol.owner)
        s"$prefix.<anon ${formatType(samType)}>"

  def format(method: BinaryMethodSymbol): String =
    method match
      case BinaryMethod(_, term, BinaryMethodKind.DefaultParameter) =>
        val sep = if !term.declaredType.isInstanceOf[MethodicType] then ": " else ""
        term.name match
          case DefaultGetterName(termName, num) =>
            s"${formatOwner(term.owner)}.$termName.<default ${num + 1}>$sep${formatType(term.declaredType)}"
      case BinaryMethod(binaryOwner, term, kind) =>
        val sep = if !term.declaredType.isInstanceOf[MethodicType] then ": " else ""
        val symbolStr = kind match
          case BinaryMethodKind.AnonFun => formatOwner(term.owner) + ".<anon fun>"
          case BinaryMethodKind.Setter => formatSymbol(term).stripSuffix("_=") + ".<setter>"
          case BinaryMethodKind.LazyInit => formatSymbol(term) + ".<lazy init>"
          case BinaryMethodKind.LocalLazyInit => formatSymbol(term) + ".<lazy init>"
          case BinaryMethodKind.MixinForwarder => formatSymbol(term) + ".<mixin forwarder>"
          case BinaryMethodKind.TraitStaticAccessor => formatSymbol(term) + ".<trait static accessor>"
          case _ => formatSymbol(term)

        s"$symbolStr$sep${formatType(term.declaredType)}"
      case BinaryOuter(owner, outer) =>
        s"${format(owner)}.<outer>: ${formatOwner(outer)}"
      case _ => throw new UnsupportedOperationException(method.toString)

  private def formatOwner(sym: Symbol): String =
    sym match
      case sym: ClassSymbol if sym.name.isPackageObject => formatSymbol(sym.owner)
      case sym: TermOrTypeSymbol => formatSymbol(sym)
      case sym: PackageSymbol => ""

  private def formatSymbol(sym: Symbol): String =
    val prefix = formatOwner(sym.owner)
    val symName = sym.name match
      case DefaultGetterName(termName, num) => s"${termName.toString()}.<default ${num + 1}>"
      case _ => sym.nameStr

    if prefix.isEmpty then symName else s"$prefix.$symName"

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
        val args = t.args.init.map(formatType).mkString(",")
        val result = formatType(t.args.last)
        if t.args.size > 2 then s"($args) => $result" else s"$args => $result"
      case t: AppliedType if t.tycon.isTuple =>
        val types = t.args.map(formatType).mkString(",")
        s"($types)"
      case t: AppliedType if t.tycon.isOperatorLike && t.args.size == 2 =>
        val operatorLikeTypeFormat = t.args
          .map(formatType)
          .mkString(
            t.tycon match
              case ref: TypeRef => s" ${ref.name} "
          )
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
