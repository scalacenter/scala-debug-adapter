package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.internal.binary
import ch.epfl.scala.debugadapter.internal.jdi.JdiMethod
import ch.epfl.scala.debugadapter.internal.stacktrace.BinaryClassSymbol.*
import ch.epfl.scala.debugadapter.internal.stacktrace.BinaryMethodSymbol.*
import tastyquery.Contexts
import tastyquery.Contexts.Context
import tastyquery.Flags
import tastyquery.Names.*
import tastyquery.Signatures.*
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.Spans.*
import tastyquery.jdk.ClasspathLoaders
import tastyquery.jdk.ClasspathLoaders.FileKind

import java.nio.file.Path
import java.util.Optional
import java.util.function.Consumer
import scala.jdk.OptionConverters.*
import scala.util.matching.Regex
import tastyquery.Modifiers.TermSymbolKind
import tastyquery.SourceLanguage
import scala.util.control.NonFatal

class Scala3Unpickler(
    classpaths: Array[Path],
    warnLogger: Consumer[String],
    testMode: Boolean
) extends ThrowOrWarn(warnLogger.accept, testMode):
  private val classpath = ClasspathLoaders.read(classpaths.toList)
  private given ctx: Context = Contexts.init(classpath)
  private val defn = new Definitions
  private val formatter = new Scala3Formatter(warnLogger.accept, testMode)

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
    findSymbol(method).flatMap(formatter.formatMethodSymbol)

  def formatClass(cls: binary.ClassType): String =
    formatter.formatClassSymbol(findClass(cls))

  def findSymbol(method: binary.Method): Option[BinaryMethodSymbol] =
    val bcls = findClass(method.declaringClass, method.isExtensionMethod)
    val cls = bcls.symbol
    cls match
      case term: TermSymbol =>
        if method.declaringClass.superclass.get.name == "scala.runtime.AbstractPartialFunction" then
          Option.when(!method.isBridge)(BinaryMethod(bcls, term, BinaryMethodKind.AnonFun))
        else
          Option.when(!method.isBridge && matchSignature(method, term))(
            BinaryMethod(bcls, term, BinaryMethodKind.AnonFun)
          )
      case cls: ClassSymbol =>
        val candidates = method match
          case LocalLazyInit(name, _) =>
            for
              owner <- withCompanionIfExtendsAnyVal(cls)
              term <- collectLocalSymbols(owner) {
                case (t: TermSymbol, None) if (t.isLazyVal || t.isModuleVal) && t.matchName(name) => t
              }
            yield BinaryMethod(bcls, term, BinaryMethodKind.LocalLazyInit)
          case AnonFun(prefix) =>
            val symbols =
              val x =
                for
                  owner <- withCompanionIfExtendsAnyVal(cls)
                  term <- collectLocalSymbols(owner) {
                    case (t: TermSymbol, None) if t.isAnonFun && matchSignature(method, t) => t
                  }
                yield term
              if x.size > 1 && prefix.nonEmpty then
                val y = x.filter(s => matchPrefix(prefix, s.owner))
                if y.size == 0 then x
                else y
              else x
            symbols.map(term => BinaryMethod(bcls, term, BinaryMethodKind.AnonFun))
          case LocalMethod(name, _) =>
            for
              owner <- withCompanionIfExtendsAnyVal(cls)
              term <- collectLocalSymbols(owner) {
                case (t: TermSymbol, None) if t.matchName(name) && matchSignature(method, t) => t
              }
            yield BinaryMethod(bcls, term, BinaryMethodKind.LocalDef)
          case LazyInit(name) =>
            cls.declarations.collect {
              case t: TermSymbol if t.isLazyVal && t.matchName(name) => BinaryMethod(bcls, t, BinaryMethodKind.LazyInit)
            }
          case _ =>
            cls.declarations
              .collect { case sym: TermSymbol => BinaryMethod(bcls, sym, BinaryMethodKind.InstanceDef) }
              .filter(bmthd => bmthd.symbol.isDefined && matchSymbol(method, bmthd.symbol.get))
        candidates.singleOptOrThrow(method.name)
      case _ => None

  def matchPrefix(prefix: String, owner: Symbol): Boolean =
    if prefix.isEmpty then true
    else if prefix.endsWith("$_") then
      val stripped = prefix.stripSuffix("$$_")
      matchPrefix(stripped, owner)
    else if prefix.endsWith("$init$") then owner.isTerm && !owner.asTerm.isMethod
    else
      val regex = owner.nameStr match
        case "$anonfun" => "\\$anonfun\\$\\d+$"
        case name =>
          Regex.quote(name)
            + (if owner.isLocal then "\\$\\d+" else "")
            + (if owner.isModuleClass then "\\$" else "")
            + "$"
      regex.r.findFirstIn(prefix) match
        case Some(suffix) =>
          def enclosingDecl(owner: Symbol): DeclaringSymbol =
            if owner.isInstanceOf[DeclaringSymbol] then owner.asInstanceOf[DeclaringSymbol]
            else enclosingDecl(owner.owner)
          val superOwner =
            if owner.isLocal && !owner.isAnonFun then enclosingDecl(owner) else owner.owner
          matchPrefix(prefix.stripSuffix(suffix).stripSuffix("$"), superOwner)
        case None => false

  def withCompanionIfExtendsAnyVal(cls: ClassSymbol): Seq[ClassSymbol] =
    cls.companionClass match
      case Some(companionClass) if companionClass.isSubclass(ctx.defn.AnyValClass) =>
        Seq(cls, companionClass)
      case _ => Seq(cls)

  def collectLocalSymbols[S](cls: ClassSymbol)(
      partialF: PartialFunction[(Symbol, Option[ClassSymbol]), S]
  ): Seq[S] =
    val f = partialF.lift.andThen(_.toSeq)

    def isInline(tree: Ident): Boolean =
      try tree.symbol.isTerm && tree.symbol.asTerm.isInline
      catch case NonFatal(e) => false

    def collectSymbols(tree: Tree, inlineSet: Set[Symbol]): Seq[S] =
      tree.walkTreeWithFilter(_ => true) {
        case ValDef(_, _, _, symbol) if symbol.isLocal && (symbol.isLazyVal || symbol.isModuleVal) => f((symbol, None))
        case DefDef(_, _, _, _, symbol) if symbol.isLocal => f(symbol, None)
        case ClassDef(_, _, symbol) if symbol.isLocal => f(symbol, None)
        case lambda: Lambda =>
          val sym = lambda.meth.asInstanceOf[TermReferenceTree].symbol
          f(sym, Some(lambda.samClassSymbol))
        case tree: Ident if isInline(tree) && !inlineSet.contains(tree.symbol) =>
          tree.symbol.tree.toSeq.flatMap(collectSymbols(_, inlineSet + tree.symbol))
        case _ => Seq.empty
      }(_ ++ _, Seq.empty)

    for
      decl <- cls.declarations
      tree <- decl.tree.toSeq
      localSym <- collectSymbols(tree, Set.empty)
    yield localSym

  def findClass(cls: binary.ClassType, isExtensionMethod: Boolean = false): BinaryClassSymbol =
    val javaParts = cls.name.split('.')
    val packageNames = javaParts.dropRight(1).toList.map(SimpleName.apply)
    val packageSym =
      if packageNames.nonEmpty
      then ctx.findSymbolFromRoot(packageNames).asInstanceOf[PackageSymbol]
      else ctx.defn.EmptyPackage
    val decodedClassName = NameTransformer.decode(javaParts.last)
    val allSymbols = decodedClassName match
      case AnonClass(declaringClassName, remaining) =>
        val WithLocalPart = "(.+)\\$(.+)\\$\\d+".r
        val decl = declaringClassName match
          case WithLocalPart(decl, _) => decl.stripSuffix("$")
          case decl => decl
        findLocalClasses(cls, packageSym, decl, "$anon", remaining)
      case LocalClass(declaringClassName, localClassName, remaining) =>
        findLocalClasses(cls, packageSym, declaringClassName, localClassName, remaining)
      case _ => findSymbolsRecursively(packageSym, decodedClassName)
    if cls.isObject && !isExtensionMethod
    then allSymbols.filter(_.symbol.isModuleClass).singleOrThrow(cls.name)
    else allSymbols.filter(!_.symbol.isModuleClass).singleOrThrow(cls.name)

  private def findLocalClasses(
      cls: binary.ClassType,
      packageSym: PackageSymbol,
      declaringClassName: String,
      localClassName: String,
      remaining: Option[String]
  ): Seq[BinaryClassSymbol] =
    val owners = findSymbolsRecursively(packageSym, declaringClassName)
    remaining match
      case None => owners.flatMap(bcls => findLocalClasses(bcls.symbol, localClassName, Some(cls)))
      case Some(remaining) =>
        val localClasses = owners
          .flatMap(t => findLocalClasses(t.symbol, localClassName, None))
          .collect { case BinaryClass(cls, _) => cls }
        localClasses.flatMap(s => findSymbolsRecursively(s, remaining))

  private def findSymbolsRecursively(owner: DeclaringSymbol, decodedName: String): Seq[BinaryClass] =
    owner.declarations
      .collect { case sym: ClassSymbol => sym }
      .flatMap { sym =>
        val Symbol = s"${Regex.quote(sym.nameStr)}\\$$?(.*)".r
        decodedName match
          case Symbol(remaining) =>
            if remaining.isEmpty then Some(BinaryClass(sym, BinaryClassKind.TopLevelOrInner))
            else findSymbolsRecursively(sym, remaining)
          case _ => None
      }

  private def findLocalClasses(
      owner: ClassSymbol,
      name: String,
      javaClass: Option[binary.ClassType]
  ): Seq[BinaryClassSymbol] =
    javaClass match
      case Some(cls) =>
        val superClassAndInterfaces =
          (cls.superclass.toSeq ++ cls.interfaces).map(findClass(_)).toList.map(_.symbol).toSet
        val x = 2
        def matchParents(classSymbol: ClassSymbol): Boolean =
          if classSymbol.isEnum then superClassAndInterfaces == classSymbol.parentClasses.toSet + ctx.defn.ProductClass
          else if cls.isInterface then superClassAndInterfaces == classSymbol.parentClasses.filter(_.isTrait).toSet
          else if classSymbol.isAnonClass then classSymbol.parentClasses.forall(superClassAndInterfaces.contains)
          else superClassAndInterfaces == classSymbol.parentClasses.toSet

        def matchSamClass(samClass: ClassSymbol): Boolean =
          if samClass == defn.partialFunction then
            superClassAndInterfaces.size == 2 &&
            superClassAndInterfaces.exists(_ == defn.abstractPartialFunction) &&
            superClassAndInterfaces.exists(_ == defn.serializable)
          else superClassAndInterfaces.contains(samClass)

        collectLocalSymbols(owner) {
          case (cls: ClassSymbol, None) if cls.matchName(name) && matchParents(cls) =>
            if name == "$anon" then BinaryClass(cls, BinaryClassKind.Anon)
            else BinaryClass(cls, BinaryClassKind.Local)
          case (lambda: TermSymbol, Some(samClass)) if matchSamClass(samClass) => BinarySAMClass(lambda, samClass)
        }
      case _ =>
        collectLocalSymbols(owner) {
          case (cls: ClassSymbol, None) if cls.matchName(name) =>
            if name == "$anon" then BinaryClass(cls, BinaryClassKind.Anon)
            else BinaryClass(cls, BinaryClassKind.Local)
          case (lambda: TermSymbol, Some(samClass)) => BinarySAMClass(lambda, samClass)
        }

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

  private def skip(bmthd: BinaryMethodSymbol): Boolean =
    (bmthd.symbol.isDefined && bmthd.symbol.get.isGetterOrSetter && !bmthd.symbol.get.isLazyValInTrait) || bmthd.symbol.get.isSynthetic
