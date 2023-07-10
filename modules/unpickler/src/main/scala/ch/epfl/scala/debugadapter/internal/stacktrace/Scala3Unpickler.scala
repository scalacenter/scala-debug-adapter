package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.internal.jdi
import tastyquery.Contexts
import tastyquery.Contexts.Context
import tastyquery.Flags
import tastyquery.Names.*
import tastyquery.Signatures.*
import tastyquery.Symbols.*
import tastyquery.Types.*
import tastyquery.jdk.ClasspathLoaders
import tastyquery.jdk.ClasspathLoaders.FileKind

import java.nio.file.Path
import java.util.function.Consumer
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.matching.Regex
import tastyquery.Types.*
import tastyquery.Signatures.*
import java.util.Optional
import scala.jdk.OptionConverters.*
import java.lang.reflect.Method

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
    findSymbolImpl(obj) match
      case Failure(exception) => throwOrWarn(exception); false
      case Success(Some(symbol)) => skip(symbol)
      case Success(None) => true

  def formatMethod(obj: Any): Optional[String] =
    val method = jdi.Method(obj)
    findSymbol(obj).map { t =>
      val notMethodType = t.declaredType match {
        case _: MethodType => false
        case _: PolyType => false
        case _ => true
      }
      val optionalString = if (notMethodType) ": " else ""
      s"${formatSymbol(t)}${optionalString}${formatType(t.declaredType)}"
    }.asJava

  private[stacktrace] def findSymbol(obj: Any): Option[TermSymbol] =
    findSymbolImpl(obj).get

  private def findSymbolImpl(obj: Any): Try[Option[TermSymbol]] =
    val method = jdi.Method(obj)
    val fqcn = method.declaringType.name
    findDeclaringType(fqcn, method.isExtensionMethod) match
      case None =>
        Failure(Exception(s"Cannot find Scala symbol of $fqcn"))
      case Some(declaringType) =>
        val matchingSymbols =
          declaringType.declarations
            .collect { case sym: TermSymbol if sym.isTerm => sym }
            .filter(matchSymbol(method, _))

        if matchingSymbols.size > 1 then
          val builder = new java.lang.StringBuilder
          builder.append(
            s"Found ${matchingSymbols.size} matching symbols for $method:"
          )
          matchingSymbols.foreach(sym => builder.append(s"\n$sym"))
          Failure(Exception(builder.toString))
        else Success(matchingSymbols.headOption)

  def formatType(t: Type): String =
    t match
      case t: MethodType =>
        val params = t.paramNames
          .zip(t.paramTypes)
          .map((n, t) => s"$n: ${formatType(t)}")
          .mkString(", ")
        val sep = if t.resultType.isInstanceOf[MethodicType] then "" else ": "
        val result = formatType(t.resultType)
        s"($params)$sep$result"
      case t: TypeRef => formatPrefix(t.prefix) + t.name
      case t: AppliedType if isFunction(t.tycon) =>
        val args = t.args.init.map(formatType).mkString(",")
        val result = formatType(t.args.last)
        if t.args.size > 2 then s"($args) => $result" else s"$args => $result"
      case t: AppliedType if isTuple(t.tycon) =>
        val types = t.args.map(formatType).mkString(",")
        s"(${types})"
      case t: AppliedType if isOperatorLike(t.tycon) && t.args.size == 2 =>
        val operatorLikeTypeFormat = t.args
          .map(formatType)
          .mkString(
            t.tycon match
              case ref: TypeRef => s" ${ref.name} "
          )
        operatorLikeTypeFormat
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
      case t: TermRefinement => formatType(t.parent) + " {...}"
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
      case _: WildcardTypeBounds => "?"
      case t: RecType => formatType(t.parent)
      case t: TypeLambda =>
        val args = t.paramNames.map(t => t.toString).mkString(", ")
        val result = formatType(t.resultType)
        s"[$args] =>> $result"
      case t @ (_: RecThis | _: SkolemType | _: SuperType | _: MatchType | _: CustomTransientGroundType |
          _: PackageRef) =>
        throwOrWarn(s"Cannot format type ${t.getClass.getName}")
        "<unsupported>"

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
    if prefix.isEmpty then sym.name.toString else s"$prefix.${sym.name}"

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

  private def extractScalaTerms(fqcn: String, isExtensionMethod: Boolean): Seq[TermSymbol] =
    for
      declaringType <- findDeclaringType(fqcn, isExtensionMethod).toSeq
      term <- declaringType.declarations
        .collect { case sym: TermSymbol if sym.isTerm => sym }
    yield term

  private def findDeclaringType(fqcn: String, isExtensionMethod: Boolean): Option[DeclaringSymbol] =
    val javaParts = fqcn.split('.')
    val isPackageObject = fqcn.endsWith(".package") || fqcn.endsWith("$package")
    val isObject = isPackageObject || fqcn.endsWith("$")
    val packageNames = javaParts.dropRight(1).toList.map(SimpleName.apply)
    val packageSym =
      if packageNames.nonEmpty
      then ctx.findSymbolFromRoot(packageNames).asInstanceOf[PackageSymbol]
      else ctx.defn.EmptyPackage
    val className = javaParts.last
    val clsSymbols = findSymbolsRecursively(packageSym, className)
    val obj = clsSymbols.filter(_.is(Flags.Module))
    val cls = clsSymbols.filter(!_.is(Flags.Module))
    assert(obj.size <= 1 && cls.size <= 1)
    if isObject && !isExtensionMethod then obj.headOption else cls.headOption

  private def findSymbolsRecursively(owner: DeclaringSymbol, encodedName: String): Seq[DeclaringSymbol] =
    owner.declarations
      .collect { case sym: DeclaringSymbol => sym }
      .flatMap { sym =>
        val encodedSymName = NameTransformer.encode(sym.name.toString)
        val Symbol = s"${Regex.quote(encodedSymName)}\\$$?(.*)".r
        encodedName match
          case Symbol(remaining) =>
            if remaining.isEmpty then Some(sym)
            else findSymbolsRecursively(sym, remaining)
          case _ => None
      }

  private def matchSymbol(method: jdi.Method, symbol: TermSymbol): Boolean =
    matchTargetName(method, symbol) && (method.isTraitInitializer || matchSignature(method, symbol))

  private def matchTargetName(method: jdi.Method, symbol: TermSymbol): Boolean =
    val javaPrefix = method.declaringType.name.replace('.', '$') + "$$"
    // if an inner accesses a private method, the backend makes the method public
    // and prefixes its name with the full class name.
    // Example: method foo in class example.Inner becomes example$Inner$$foo
    val expectedName = method.name.stripPrefix(javaPrefix)
    val symbolName = symbol.targetName.toString
    val encodedScalaName = symbolName match
      case "<init>" if symbol.owner.is(Flags.Trait) => "$init$"
      case "<init>" => "<init>"
      case _ => NameTransformer.encode(symbolName)
    if method.isExtensionMethod then encodedScalaName == expectedName.stripSuffix("$extension")
    else encodedScalaName == expectedName

  private def matchSignature(method: jdi.Method, symbol: TermSymbol): Boolean =
    try
      symbol.signedName match
        case SignedName(_, sig, _) =>
          val javaArgs = method.arguments.headOption.map(_.name) match
            case Some("$this") if method.isExtensionMethod => method.arguments.tail
            case Some("$outer") if method.isClassInitializer => method.arguments.tail
            case _ => method.arguments
          matchArguments(sig.paramsSig, javaArgs) &&
          method.returnType.forall(returnType => {
            val javaRetType =
              if (method.isClassInitializer) method.declaringType else returnType
            matchType(sig.resSig, javaRetType)
          })
        case _ =>
          true // TODO compare symbol.declaredType
    catch
      case e: UnsupportedOperationException =>
        warn(e.getMessage)
        true

  private def matchArguments(scalaArgs: Seq[ParamSig], javaArgs: Seq[jdi.LocalVariable]): Boolean =
    scalaArgs
      .collect { case termSig: ParamSig.Term => termSig }
      .corresponds(javaArgs) { (scalaArg, javaArg) => matchType(scalaArg.typ, javaArg.`type`) }

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
      javaType: jdi.Type
  ): Boolean =
    def rec(scalaType: String, javaType: String): Boolean =
      scalaType match
        case "scala.Any[]" =>
          javaType == "java.lang.Object[]" || javaType == "java.lang.Object"
        case s"$scalaType[]" => rec(scalaType, javaType.stripSuffix("[]"))
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
      (!symbol.flags.is(Flags.Method) || symbol.is(Flags.Accessor)) &&
        !symbol.is(Flags.Lazy)
    isNonLazyGetterOrSetter || symbol.is(Flags.Synthetic)
