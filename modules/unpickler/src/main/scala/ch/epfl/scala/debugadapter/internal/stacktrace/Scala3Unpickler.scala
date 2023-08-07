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
    val symbol = findSymbol(method)
    method match
      case LazyInit(name) => symbol.isEmpty
      case _ => symbol.forall(skip)

  def formatMethod(obj: Any): Optional[String] =
    formatMethod(JdiMethod(obj)).toJava

  def formatMethod(method: binary.Method): Option[String] =
    findSymbol(method).map { symbol =>
      val sep = if !symbol.declaredType.isInstanceOf[MethodicType] then ": " else ""
      s"${formatSymbol(symbol)}$sep${formatType(symbol.declaredType)}"
    }
  def formatClass(cls: binary.ClassType): String =
    formatSymbol(findClass(cls))

  def findSymbol(method: binary.Method): Option[TermSymbol] =
    val cls = findClass(method.declaringClass, method.isExtensionMethod)
    val candidates = method match
      case LocalLazyInit(name, _) =>
        for
          owner <- withCompanionIfExtendsAnyVal(cls)
          term <- collectLocalSymbols(owner) {
            case t: TermSymbol if (t.isLazyVal || t.isModuleVal) && t.matchName(name) => t
          }
        yield term
      case LocalMethod(name, _) =>
        for
          owner <- withCompanionIfExtendsAnyVal(cls)
          term <- collectLocalSymbols(owner) {
            case t: TermSymbol if t.matchName(name) && matchSignature(method, t) => t
          }
        yield term
      case LazyInit(name) =>
        cls.declarations.collect { case t: TermSymbol if t.isLazyVal && t.matchName(name) => t }
      case _ =>
        cls.declarations
          .collect { case sym: TermSymbol => sym }
          .filter(matchSymbol(method, _))
    candidates.singleOptOrThrow(method.name)

  def withCompanionIfExtendsAnyVal(cls: ClassSymbol): Seq[ClassSymbol] =
    cls.companionClass match
      case Some(companionClass) if companionClass.isSubclass(ctx.defn.AnyValClass) =>
        Seq(cls, companionClass)
      case _ => Seq(cls)

  def collectLocalSymbols[S <: Symbol](cls: ClassSymbol)(partialF: PartialFunction[Symbol, S]): Seq[S] =
    val f = partialF.lift.andThen(_.toSeq)

    def collectSymbols(tree: Tree): Seq[S] =
      tree.walkTree {
        case ValDef(_, _, _, symbol) if symbol.isLocal && (symbol.isLazyVal || symbol.isModuleVal) => f(symbol)
        case DefDef(_, _, _, _, symbol) if symbol.isLocal => f(symbol)
        case ClassDef(_, _, symbol) if symbol.isLocal => f(symbol)
        case _ => Seq.empty
      }(_ ++ _, Seq.empty)

    for
      decl <- cls.declarations
      tree <- decl.tree.toSeq
      localSym <- collectSymbols(tree)
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

  def findClass(cls: binary.ClassType, isExtensionMethod: Boolean = false): ClassSymbol =
    val javaParts = cls.name.split('.')
    val packageNames = javaParts.dropRight(1).toList.map(SimpleName.apply)
    val packageSym =
      if packageNames.nonEmpty
      then ctx.findSymbolFromRoot(packageNames).asInstanceOf[PackageSymbol]
      else ctx.defn.EmptyPackage
    val decodedClassName = NameTransformer.decode(javaParts.last)
    val allSymbols = decodedClassName match
      case LocalClass(declaringClassName, localClassName, remaining) =>
        val owners = findSymbolsRecursively(packageSym, declaringClassName)
        val localClasses = owners.flatMap(findLocalClasses(_, localClassName, cls))
        remaining match
          case None => localClasses
          case Some(remaining) => localClasses.flatMap(findSymbolsRecursively(_, remaining))
      case _ => findSymbolsRecursively(packageSym, decodedClassName)
    if cls.isObject && !isExtensionMethod
    then allSymbols.filter(_.isModuleClass).singleOrThrow(cls.name)
    else allSymbols.filter(!_.isModuleClass).singleOrThrow(cls.name)

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

  private def findLocalClasses(owner: ClassSymbol, name: String, cls: binary.ClassType): Seq[ClassSymbol] =
    val superClassAndInterfaces = (cls.superclass.toSeq ++ cls.interfaces).map(findClass(_)).toSet

    def matchesParents(classSymbol: ClassSymbol): Boolean =
      if classSymbol.isEnum then superClassAndInterfaces == classSymbol.parentClasses.toSet + ctx.defn.ProductClass
      else if cls.isInterface then superClassAndInterfaces == classSymbol.parentClasses.filter(_.isTrait).toSet
      else superClassAndInterfaces == classSymbol.parentClasses.toSet

    collectLocalSymbols(owner) { case cls: ClassSymbol if cls.matchName(name) && matchesParents(cls) => cls }

  private def matchSymbol(method: binary.Method, symbol: TermSymbol): Boolean =
    matchTargetName(method, symbol) && (method.isTraitInitializer || matchSignature(method, symbol))

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
    def parametersName(tpe: TypeOrMethodic): List[String] =
      tpe match
        case t: MethodType =>
          t.paramNames.map(_.toString()) ++ parametersName(t.resultType)
        case t: PolyType =>
          parametersName(t.resultType)
        case _ => List()

    def matchesCapture(paramName: String) =
      val pattern = ".+\\$\\d+".r
      pattern.matches(
        paramName
      ) || (method.isExtensionMethod && paramName == "$this") || (method.isClassInitializer && paramName == "$outer")

    val paramNames: List[String] = parametersName(symbol.declaredType)
    val capturedParams = method.allParameters.dropRight(paramNames.size)
    val declaredParams = method.allParameters.drop(capturedParams.size)
    capturedParams.map(_.name).forall(matchesCapture) &&
    declaredParams.map(_.name).corresponds(paramNames)((n1, n2) => n1 == n2) &&
    (symbol.signedName match
      case SignedName(_, sig, _) =>
        matchArgumentsTypes(sig.paramsSig, declaredParams)
        && method.declaredReturnType.forall(matchType(sig.resSig, _))
      case _ =>
        // TODO compare symbol.declaredType
        declaredParams.isEmpty
    )

  private def matchArgumentsTypes(scalaParams: Seq[ParamSig], javaParams: Seq[binary.Parameter]): Boolean =
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
    (symbol.isGetterOrSetter && !symbol.isLazyValInTrait) || symbol.isSynthetic

  extension (symbol: TermSymbol)
    private def isGetterOrSetter = !symbol.isMethod || symbol.isSetter
    private def isLazyValInTrait: Boolean = symbol.owner.isTrait && symbol.isLazyVal
    private def isLazyVal: Boolean = symbol.kind == TermSymbolKind.LazyVal

  extension (symbol: Symbol)
    private def isTrait = symbol.isClass && symbol.asClass.isTrait
    private def matchName(name: String) = symbol.name.toString == name
    private def isLocal = symbol.owner.isTerm

  extension [T <: Symbol](symbols: Seq[T])
    def singleOrThrow(binaryName: String): T =
      singleOptOrThrow(binaryName)
        .getOrElse(throw new NotFoundException(s"Cannot find Scala symbol of $binaryName"))

    def singleOptOrThrow(binaryName: String): Option[T] =
      if symbols.size > 1 then throw new AmbiguousException(s"Found ${symbols.size} matching symbols for $binaryName")
      else symbols.headOption

case class AmbiguousException(m: String) extends Exception
case class NotFoundException(m: String) extends Exception
