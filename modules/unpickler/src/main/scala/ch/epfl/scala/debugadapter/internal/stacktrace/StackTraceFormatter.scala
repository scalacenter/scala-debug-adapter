package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Types.*
import tastyquery.Names.*
import ch.epfl.scala.debugadapter.internal.stacktrace.*
import tastyquery.Contexts.Context
import tastyquery.Modifiers.TermSymbolKind

class StackTraceFormatter(using ThrowOrWarn):
  def format(cls: DecodedClass): String =
    cls match
      case cls: DecodedClass.ClassDef => formatQualifiedName(cls.symbol)
      case cls: DecodedClass.SAMOrPartialFunction =>
        formatQualifiedName(cls.symbol.owner).dot(s"<anon ${formatQualifiedName(cls.parentClass)}>")
      case cls: DecodedClass.SyntheticCompanionClass => formatQualifiedName(cls.companionSymbol)
      case cls: DecodedClass.InlinedClass => format(cls.underlying)

  def format(method: DecodedMethod): String =
    val typeAscription = method.declaredType match
      case tpe: Type => ": " + format(tpe)
      case tpe => format(tpe)
    formatOwner(method).dot(formatName(method)) + typeAscription

  private def formatOwner(method: DecodedMethod): String =
    method match
      case method: DecodedMethod.ValOrDefDef => formatOwner(method.symbol)
      case method: DecodedMethod.LazyInit =>
        if method.symbol.owner.isTrait then
          // the lazy init is in subclass
          format(method.owner)
        else formatOwner(method.symbol)
      case method: DecodedMethod.TraitParamAccessor => format(method.owner)
      case method: DecodedMethod.MixinForwarder => format(method.owner)
      case method: DecodedMethod.TraitStaticForwarder => formatOwner(method.target)
      case method: DecodedMethod.OuterAccessor => format(method.owner)
      case method: DecodedMethod.SuperConstructorArg => formatQualifiedName(method.treeOwner)
      case method: DecodedMethod.LiftedTry => formatQualifiedName(method.treeOwner)
      case method: DecodedMethod.ByNameArg => formatQualifiedName(method.treeOwner)
      case method: DecodedMethod.Bridge => formatOwner(method.target)
      case method: DecodedMethod.SAMOrPartialFunctionImpl => format(method.owner)
      case method: DecodedMethod.StaticForwarder => format(method.owner)
      case method: DecodedMethod.DeserializeLambda => format(method.owner)
      case method: DecodedMethod.SetterAccessor => format(method.owner)
      case method: DecodedMethod.GetterAccessor => format(method.owner)
      case method: DecodedMethod.SuperAccessor => format(method.owner)
      case method: DecodedMethod.SpecializedMethod => formatOwner(method.symbol)
      case method: DecodedMethod.InlineAccessor => format(method.owner)
      case method: DecodedMethod.AdaptedFun => formatOwner(method.target)
      case method: DecodedMethod.SAMOrPartialFunctionConstructor => format(method.owner)
      case method: DecodedMethod.InlinedMethod => formatOwner(method.underlying)

  private def formatName(method: DecodedMethod): String =
    method match
      case method: DecodedMethod.ValOrDefDef => formatName(method.symbol)
      case method: DecodedMethod.LazyInit => formatName(method.symbol).dot("<lazy init>")
      case method: DecodedMethod.TraitParamAccessor => formatName(method.symbol)
      case method: DecodedMethod.MixinForwarder => formatName(method.target).dot("<mixin forwarder>")
      case method: DecodedMethod.TraitStaticForwarder => formatName(method.target).dot("<static forwarder>")
      case _: DecodedMethod.OuterAccessor => "<outer>"
      case method: DecodedMethod.SuperConstructorArg => "<init>.<super arg>"
      case method: DecodedMethod.LiftedTry => "<try>"
      case _: DecodedMethod.ByNameArg => "<by-name arg>"
      case method: DecodedMethod.Bridge => formatName(method.target).dot("<bridge>")
      case method: DecodedMethod.SAMOrPartialFunctionImpl => formatName(method.implementedSymbol)
      case method: DecodedMethod.StaticForwarder => formatName(method.target).dot("<static forwarder>")
      case _: DecodedMethod.DeserializeLambda => "$deserializeLambda$"
      case method: DecodedMethod.SetterAccessor =>
        if method.symbol.isMethod then formatName(method.symbol) else formatName(method.symbol) + "_="
      case method: DecodedMethod.GetterAccessor => formatName(method.symbol)
      case method: DecodedMethod.SuperAccessor => formatName(method.symbol).dot("<super>")
      case method: DecodedMethod.SpecializedMethod => formatName(method.symbol).dot("<specialized>")
      case method: DecodedMethod.InlineAccessor =>
        s"<inline ${formatOwner(method.target).dot(formatName(method.target))}>"
      case method: DecodedMethod.AdaptedFun => formatName(method.target).dot("<adapted>")
      case _: DecodedMethod.SAMOrPartialFunctionConstructor => "<init>"
      case method: DecodedMethod.InlinedMethod => formatName(method.underlying)

  private def formatQualifiedName(sym: Symbol): String =
    formatOwner(sym).dot(formatName(sym))

  private def formatOwner(sym: Symbol): String =
    sym.owner match
      case owner: ClassSymbol =>
        if owner.name.isPackageObjectClass then format(owner.owner.name)
        else formatOwner(owner).dot(formatName(owner))
      case owner: TermSymbol =>
        if owner.kind == TermSymbolKind.Def then formatOwner(owner).dot(formatName(owner))
        else formatOwner(owner)
      case _ => ""

  private def formatName(sym: Symbol): String =
    sym match
      case sym: ClassSymbol if sym.name.isPackageObjectClass => format(sym.owner.name)
      case _ => format(sym.name)

  extension (prefix: String)
    def dot(suffix: String): String =
      if prefix.nonEmpty && suffix.nonEmpty then s"$prefix.$suffix" else prefix + suffix

  private def format(name: Name): String =
    def rec(name: Name): String = name match
      case DefaultGetterName(termName, num) => s"${termName.toString()}.<default ${num + 1}>"
      case name: TypeName => rec(name.toTermName)
      case SimpleName("$anonfun") => "<anon fun>"
      case SimpleName("$anon") => "<anon class>"
      case ObjectClassName(underlying) => rec(underlying)
      case UniqueName(SimpleName(""), _, _) => "<anon>"
      case _ => name.toString
    rec(name)

  private def format(t: TypeOrMethodic | TypeOrWildcard): String =
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
      case t: TypeRef => formatPrefix(t.prefix) + format(t.name)
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
      case t: RepeatedType => format(t.elemType) + "*"
      case t: TypeRefinement => format(t.parent) + " {...}"
      case t: RecType => format(t.parent)
      case _: WildcardTypeArg => "?"
      case t: TypeLambda =>
        val args = t.paramNames.map(t => t.toString).mkString(", ")
        val result = format(t.resultType)
        s"[$args] =>> $result"
      case t: NothingType => "Nothing"
      case t: AnyKindType => "AnyKind"
      case t @ (_: RecThis | _: SkolemType | _: SuperType | _: MatchType | _: CustomTransientGroundType) =>
        throwOrWarn(s"Cannot format type ${t.getClass.getName}")
        "<unsupported>"

  private def formatPolymorphicFunction(t: TypeOrMethodic): String =
    t match
      case t: PolyType =>
        val args = t.paramNames.mkString(", ")
        val result = formatPolymorphicFunction(t.resultType)
        s"[$args] => $result"
      case t: MethodType =>
        val params = t.paramTypes.map(format(_)).mkString(", ")
        if t.paramTypes.size > 1 then s"($params) => ${format(t.resultType)}"
        else s"$params => ${format(t.resultType)}"
      case t: Type =>
        // for exhaustivity
        format(t)

  private def formatPrefix(p: Prefix): String =
    val prefix = p match
      case NoPrefix => ""
      case p: TermRef if isScalaPredef(p) => ""
      case p: TermRef if isPackageObject(p.name) => ""
      case p: TermRef => formatPrefix(p.prefix) + format(p.name)
      case p: TermParamRef => p.paramName.toString
      case p: PackageRef => ""
      case p: ThisType => ""
      case t: Type => format(t)

    if prefix.nonEmpty then s"$prefix." else prefix
