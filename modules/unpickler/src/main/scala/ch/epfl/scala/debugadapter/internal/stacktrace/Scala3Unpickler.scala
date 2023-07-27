package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.internal.binary
import ch.epfl.scala.debugadapter.internal.jdi.JdiMethod
import tastyquery.Contexts
import tastyquery.Contexts.Context
import tastyquery.Definitions
import tastyquery.Flags
import tastyquery.Names.*
import tastyquery.Signatures.*
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.jdk.ClasspathLoaders
import tastyquery.jdk.ClasspathLoaders.FileKind

import java.nio.file.Path
import java.util.Optional
import java.util.function.Consumer
import scala.jdk.OptionConverters.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.matching.Regex
import tastyquery.Modifiers.TermSymbolKind
import tastyquery.SourceLanguage

class Scala3Unpickler(
    classpaths: Array[Path],
    warnLogger: Consumer[String],
    testMode: Boolean
):
  private val classpath = ClasspathLoaders.read(classpaths.toList)
  private given ctx: Context = Contexts.init(classpath)

  private def warn(msg: String): Unit = warnLogger.accept(msg)

  private def throwOrWarn(message: String): Unit =
    throwOrWarn(new Exception(message))

  private def throwOrWarn(exception: Throwable): Unit =
    if testMode then throw exception
    else exception.getMessage

  def skipMethod(obj: Any): Boolean =
    skipMethod(JdiMethod(obj): binary.Method)

  def skipMethod(method: binary.Method): Boolean =
    findSymbol(method).forall(skip)

  def formatMethod(obj: Any): Optional[String] =
    formatMethod(JdiMethod(obj)).toJava

  def formatMethod(method: binary.Method): Option[String] =
    findSymbol(method).map { symbol =>
      val sep = if !symbol.declaredType.isInstanceOf[MethodicType] then ": " else ""
      s"${formatSymbol(symbol)}$sep${formatType(symbol.declaredType)}"
    }
  def formatClass(cls: binary.ClassType): Option[String] =
    findClass(cls).map(formatSymbol(_))

  private[stacktrace] def findSymbol(obj: Any): Option[TermSymbol] =
    findSymbol(JdiMethod(obj))

  private[stacktrace] def findSymbol(method: binary.Method): Option[TermSymbol] =
    findClass(method.declaringClass, method.isExtensionMethod) match
      case None => throw new Exception(s"Cannot find Scala symbol of ${method.declaringClass.name}")
      case Some(declaringClass) =>
        matchesLocalMethodOrLazyVal(method) match
          case Some((name, _)) =>
            localMethodsAndLazyVals(declaringClass, NameTransformer.decode(name))
              .filter(matchSignature(method, _))
              .singleOrThrow(method)
          case None =>
            declaringClass.declarations
              .collect { case sym: TermSymbol if sym.isTerm => sym }
              .filter(matchSymbol(method, _))
              .singleOrThrow(method)

  extension (symbols: Seq[TermSymbol])
    def singleOrThrow(method: binary.Method): Option[TermSymbol] =
      if symbols.size > 1 then
        val message = s"Found ${symbols.size} matching symbols for $method:" +
          symbols.mkString("\n")
        throw new Exception(message)
      else symbols.headOption

  def localMethodsAndLazyVals(declaringClass: ClassSymbol, name: String): Seq[TermSymbol] =
    def matchName(symbol: Symbol): Boolean = symbol.name.toString == name
    def isLocal(symbol: Symbol): Boolean = symbol.owner.isTerm

    def findLocalMethodsOrLazyVals(tree: Tree): Seq[TermSymbol] =
      tree.walkTree {
        case DefDef(_, _, _, _, symbol) if matchName(symbol) && isLocal(symbol) => Seq(symbol)
        case ValDef(_, _, _, symbol) if matchName(symbol) && isLocal(symbol) => Seq(symbol)
        case _ => Seq.empty
      }(_ ++ _, Seq.empty)

    val declaringClasses = declaringClass.companionClass match
      case Some(companionClass) if companionClass.isSubclass(ctx.defn.AnyValClass) =>
        Seq(declaringClass, companionClass)
      case _ => Seq(declaringClass)

    for
      declaringSym <- declaringClasses
      decl <- declaringSym.declarations
      tree <- decl.tree.toSeq
      localSym <- findLocalMethodsOrLazyVals(tree)
    yield localSym

  def formatType(t: TermType | TypeOrWildcard): String =
    t match
      case t: MethodType =>
        val params = t.paramNames
          .map(paramName =>
            val pattern: Regex = """.+\$\d+$""".r
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
      case t: AppliedType if isFunction(t.tycon) =>
        val args = t.args.init.map(formatType).mkString(",")
        val result = formatType(t.args.last)
        if t.args.size > 2 then s"($args) => $result" else s"$args => $result"
      case t: AppliedType if isTuple(t.tycon) =>
        val types = t.args.map(formatType).mkString(",")
        s"($types)"
      case t: AppliedType if isOperatorLike(t.tycon) && t.args.size == 2 =>
        val operatorLikeTypeFormat = t.args
          .map(formatType)
          .mkString(
            t.tycon match
              case ref: TypeRef => s" ${ref.name} "
          )
        operatorLikeTypeFormat
      case t: AppliedType if isVarArg(t.tycon) =>
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

  private def formatSymbol(sym: Symbol): String =
    val prefix = sym.owner match
      case owner: ClassSymbol if isPackageObject(owner.name) => formatSymbol(owner.owner)
      case owner: TermOrTypeSymbol => formatSymbol(owner)
      case owner: PackageSymbol => ""
    val symName = sym.name match
      case DefaultGetterName(termName, num) => s"${termName.toString()}.<default ${num + 1}>"
      case _ => sym.name.toString()

    if prefix.isEmpty then symName else s"$prefix.$symName"

  private def isPackageObject(name: Name): Boolean =
    name.toString == "package" || name.toString.endsWith("$package")

  private def isScalaPredef(ref: TermRef): Boolean =
    isScalaPackage(ref.prefix) && ref.name.toString == "Predef"

  private def isFunction(tpe: Type): Boolean =
    tpe match
      case ref: TypeRef =>
        isScalaPackage(ref.prefix) && ref.name.toString.startsWith("Function")
      case _ => false

  private def isTuple(tpe: Type): Boolean =
    tpe match
      case ref: TypeRef =>
        isScalaPackage(ref.prefix) && ref.name.toString.startsWith("Tuple")
      case _ => false
  private def isVarArg(tpe: Type): Boolean =
    tpe match
      case ref: TypeRef =>
        isScalaPackage(ref.prefix) && ref.name.toString == "<repeated>"
      case _ => false

  private def isOperatorLike(tpe: Type): Boolean =
    tpe match
      case ref: TypeRef =>
        val operatorChars = "\\+\\-\\*\\/\\%\\&\\|\\^\\<\\>\\=\\!\\~\\#\\:\\@\\?"
        val regex = s"[^$operatorChars]".r
        !regex.findFirstIn(ref.name.toString).isDefined
      case _ => false

  private def isScalaPackage(prefix: Prefix): Boolean =
    prefix match
      case p: PackageRef => p.fullyQualifiedName.toString == "scala"
      case _ => false

  def findClass(cls: binary.ClassType, isExtensionMethod: Boolean = false): Option[ClassSymbol] =
    val javaParts = cls.name.split('.')
    val packageNames = javaParts.dropRight(1).toList.map(SimpleName.apply)
    val packageSym =
      if packageNames.nonEmpty
      then ctx.findSymbolFromRoot(packageNames).asInstanceOf[PackageSymbol]
      else ctx.defn.EmptyPackage
    val className = NameTransformer.decode(javaParts.last)
    val clsSymbols = className match
      case LocalClass(declCls, localCls, remaining) =>
        val sym = findLocalClasses(findSymbolsRecursively(packageSym, declCls), localCls, cls)
        remaining match
          case None => sym
          case Some(remaining) => sym.flatMap(findSymbolsRecursively(_, remaining))
      case _ => findSymbolsRecursively(packageSym, className)
    val objSym = clsSymbols.filter(_.isModuleClass)
    val clsSym = clsSymbols.filter(!_.isModuleClass)
    if objSym.size > 1 || clsSym.size > 1 then throw Exception("more than one")
    if cls.isObject && !isExtensionMethod then objSym.headOption else clsSym.headOption

  object LocalClass:
    def unapply(name: String): Option[(String, String, Option[String])] =
      "(.+)\\$([^$]+)\\$\\d+(\\$.*)?".r
        .unapplySeq(name)
        .map(xs => (xs(0), xs(1), Option(xs(2)).map(_.stripPrefix("$")).filter(_.nonEmpty)))

  private def findSymbolsRecursively(owner: DeclaringSymbol, decodedName: String): Seq[ClassSymbol] =
    owner.declarations
      .collect { case sym: ClassSymbol => sym }
      .flatMap { sym =>
        val Symbol = s"${Regex.quote(sym.name.toString)}\\$$?(.*)".r
        decodedName match
          case Symbol(remaining) =>
            if remaining.isEmpty then Some(sym)
            else findSymbolsRecursively(sym, remaining)
          case _ => None
      }

  private def findLocalClasses(owners: Seq[ClassSymbol], name: String, cls: binary.ClassType): Seq[ClassSymbol] =
    def matchName(symbol: Symbol): Boolean = symbol.name.toString == name
    def isLocal(symbol: Symbol): Boolean = symbol.owner.isTerm
    val superClassAndInterfaces =
      (cls.superclass.toSeq ++ cls.interfaces)
        .map(p => findClass(p).getOrElse(throw Exception(s"Cannot find symbol for parent $p")))
        .toSet

    def matchesParents(classSymbol: ClassSymbol): Boolean =
      val symbolParents =
        if !cls.isInterface then classSymbol.parentClasses
        else classSymbol.parentClasses.filter(_.isTrait)
      superClassAndInterfaces == symbolParents.toSet
    def findLocalClasses(tree: Tree): Seq[ClassSymbol] =
      tree.walkTree {
        case ClassDef(_, _, symbol) if matchName(symbol) && isLocal(symbol) && matchesParents(symbol) => Seq(symbol)
        case _ => Seq.empty
      }(_ ++ _, Seq.empty)

    for
      owner <- owners
      tree <- owner.tree.toSeq
      localClass <- findLocalClasses(tree)
    yield localClass

  private def matchSymbol(method: binary.Method, symbol: TermSymbol): Boolean =
    matchTargetName(method, symbol) && (method.isTraitInitializer || matchSignature(method, symbol))

  private def matchesLocalMethodOrLazyVal(method: binary.Method): Option[(String, Int)] =
    val javaPrefix = method.declaringClass.name.replace('.', '$') + "$$"
    val expectedName = method.name.stripPrefix(javaPrefix).split("\\$_\\$").last
    val localMethod = "(.+)\\$(\\d+)".r
    val lazyInit = "(.+)\\$lzyINIT\\d+\\$(\\d+)".r
    expectedName match
      case lazyInit(name, index) => Some((name, index.toInt))
      case localMethod(name, index) if !name.endsWith("$default") => Some((name, index.toInt))
      case _ => None

  private def matchTargetName(method: binary.Method, symbol: TermSymbol): Boolean =
    val javaPrefix = method.declaringClass.name.replace('.', '$') + "$$"
    // if an inner accesses a private method, the backend makes the method public
    // and prefixes its name with the full class name.
    // Example: method foo in class example.Inner becomes example$Inner$$foo
    val expectedName = method.name.stripPrefix(javaPrefix)
    val symbolName = symbol.targetName.toString
    val encodedScalaName = symbolName match
      case "<init>" if symbol.owner.asClass.isTrait => "$init$"
      case "<init>" => "<init>"
      case _ => NameTransformer.encode(symbolName)
    if method.isExtensionMethod then encodedScalaName == expectedName.stripSuffix("$extension")
    else encodedScalaName == expectedName

  private def matchSignature(method: binary.Method, symbol: TermSymbol): Boolean =
    symbol.signedName match
      case SignedName(_, sig, _) =>
        matchArguments(sig.paramsSig, method.declaredParams)
        && method.declaredReturnType.forall(matchType(sig.resSig, _))
      case _ =>
        // TODO compare symbol.declaredType
        method.declaredParams.isEmpty

  private def matchArguments(scalaParams: Seq[ParamSig], javaParams: Seq[binary.Parameter]): Boolean =
    scalaParams
      .collect { case termSig: ParamSig.Term => termSig }
      .corresponds(javaParams)((scalaParam, javaParam) => matchType(scalaParam.typ, javaParam.`type`))

  private val javaToScala: Map[String, String] = Map(
    "scala.Boolean" -> "boolean",
    "scala.Byte" -> "byte",
    "scala.Char" -> "char",
    "scala.Double" -> "double",
    "scala.Float" -> "float",
    "scala.Int" -> "int",
    "scala.Long" -> "long",
    "scala.Short" -> "short",
    "scala.Unit" -> "void",
    "scala.Any" -> "java.lang.Object",
    "scala.Null" -> "scala.runtime.Null$",
    "scala.Nothing" -> "scala.runtime.Nothing$"
  )

  private def matchType(
      scalaType: FullyQualifiedName,
      javaType: binary.Type
  ): Boolean =
    def rec(scalaType: String, javaType: String): Boolean =
      scalaType match
        case "scala.Any[]" =>
          javaType == "java.lang.Object[]" || javaType == "java.lang.Object"
        case "scala.PolyFunction" =>
          val regex = s"${Regex.quote("scala.Function")}\\d+".r
          regex.matches(javaType)
        case s"$scalaType[]" => rec(scalaType, javaType.stripSuffix("[]"))
        case s"$scalaOwner._$$$classSig" =>
          val parts = classSig
            .split(Regex.quote("_$"))
            .last
            .split('.')
            .map(NameTransformer.encode)
            .map(Regex.quote)
          val regex = ("\\$" + parts.head + "\\$\\d+\\$" + parts.tail.map(_ + "\\$").mkString + "?" + "$").r
          regex.findFirstIn(javaType).exists { suffix =>
            val prefix = javaType.stripSuffix(suffix).replace('$', '.')
            scalaOwner.startsWith(prefix)
          }

        case _ =>
          val regex = scalaType
            .split('.')
            .map(NameTransformer.encode)
            .map(Regex.quote)
            .mkString("", "[\\.\\$]", "\\$?")
            .r
          javaToScala
            .get(scalaType)
            .map(_ == javaType)
            .getOrElse(regex.matches(javaType))
    rec(scalaType.toString, javaType.name)

  private def skip(symbol: TermSymbol): Boolean =
    val isNonLazyGetterOrSetter =
      (!symbol.isMethod || symbol.isSetter) && symbol.kind != TermSymbolKind.LazyVal
    isNonLazyGetterOrSetter || symbol.isSynthetic
