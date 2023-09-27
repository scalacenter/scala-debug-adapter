package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.internal.binary
import ch.epfl.scala.debugadapter.internal.jdi.JdiMethod
import ch.epfl.scala.debugadapter.internal.stacktrace.BinaryClassSymbol.*
import ch.epfl.scala.debugadapter.internal.stacktrace.BinaryMethodSymbol.*
import ch.epfl.scala.debugadapter.internal.stacktrace.BinaryMethodKind.*
import ch.epfl.scala.debugadapter.internal.stacktrace.BinaryClassKind.*
import tastyquery.Contexts
import tastyquery.Contexts.Context
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
import scala.util.control.NonFatal
import tastyquery.Traversers.TreeTraverser
import scala.collection.mutable.Buffer

class Scala3Unpickler(
    classpaths: Array[Path],
    warnLogger: Consumer[String],
    testMode: Boolean
) extends ThrowOrWarn(warnLogger.accept, testMode):
  private val classpath = ClasspathLoaders.read(classpaths.toList)
  private given ctx: Context = Contexts.init(classpath)
  private val defn = new Definitions
  private[stacktrace] val formatter = new Scala3Formatter(warnLogger.accept, testMode)

  def skipMethod(obj: Any): Boolean =
    skipMethod(JdiMethod(obj): binary.Method)

  def skipMethod(method: binary.Method): Boolean =
    try
      val symbol = findMethod(method)
      skip(findMethod(method))
    catch case _ => true

  def formatMethod(obj: Any): Optional[String] =
    formatMethod(JdiMethod(obj)).toJava

  def formatMethod(method: binary.Method): Option[String] =
    findMethod(method) match
      case BinaryMethod(_, _, MixinForwarder | TraitStaticAccessor) =>
        None
      case binaryMethod => Some(formatter.format(binaryMethod))

  def formatClass(cls: binary.ClassType): String =
    formatter.format(findClass(cls))

  def findMethod(method: binary.Method): BinaryMethodSymbol =
    val binaryClass = findClass(method.declaringClass, method.isExtensionMethod)
    binaryClass match
      case BinarySAMClass(term, _, _) =>
        if method.declaringClass.superclass.get.name == "scala.runtime.AbstractPartialFunction" then
          if !method.isBridge then BinaryMethod(binaryClass, term, AnonFun)
          else notFound(method)
        else if !method.isBridge && matchSignature(method, term) then BinaryMethod(binaryClass, term, AnonFun)
        else notFound(method)
      case binaryClass: BinaryClass =>
        val candidates = method match
          case Patterns.LocalLazyInit(name, _) =>
            collectLocalMethods(binaryClass, LocalLazyInit, method.sourceLines) {
              case (t: TermSymbol, None) if (t.isLazyVal || t.isModuleVal) && t.matchName(name) => t
            }
          case Patterns.AnonFun(prefix) =>
            collectLocalMethods(binaryClass, AnonFun, method.sourceLines) {
              case (t: TermSymbol, None) if t.isAnonFun && matchSignature(method, t) => t
            }
          case Patterns.LocalMethod(name, _) =>
            collectLocalMethods(binaryClass, LocalDef, method.sourceLines) {
              case (t: TermSymbol, None) if t.matchName(name) && matchSignature(method, t) => t
            }
          case Patterns.LazyInit(name) =>
            binaryClass.symbol.declarations.collect {
              case t: TermSymbol if t.isLazyVal && t.matchName(name) => BinaryMethod(binaryClass, t, LazyInit)
            }
          case Patterns.StaticAccessor(_) =>
            binaryClass.symbol.declarations.collect {
              case sym: TermSymbol if matchSymbol(method, sym) =>
                BinaryMethod(binaryClass, sym, TraitStaticAccessor)
            }
          case Patterns.Outer(_) =>
            def outerClass(sym: Symbol): ClassSymbol =
              if sym.owner.isClass then sym.owner.asClass
              else outerClass(sym.owner)
            val outer = binaryClass.symbol.owner.owner
            List(BinaryOuter(binaryClass, outerClass(binaryClass.symbol)))

          case _ =>
            val candidates = binaryClass.symbol.declarations
              .collect {
                case sym: TermSymbol if matchSymbol(method, sym) =>
                  if method.name == "$init$" then BinaryMethod(binaryClass, sym, TraitConstructor)
                  else if method.name == "<init>" then BinaryMethod(binaryClass, sym, Constructor)
                  else if !sym.isMethod then BinaryMethod(binaryClass, sym, Getter)
                  else if sym.isSetter then BinaryMethod(binaryClass, sym, Setter)
                  else if method.name.contains("$default$") then BinaryMethod(binaryClass, sym, DefaultParameter)
                  else BinaryMethod(binaryClass, sym, InstanceDef)
              }
            if candidates.nonEmpty then candidates
            else
              def allTraitParents(cls: ClassSymbol): Seq[ClassSymbol] =
                (cls.parentClasses ++ cls.parentClasses.flatMap(allTraitParents)).distinct.filter(_.isTrait)
              allTraitParents(binaryClass.symbol)
                .flatMap(parent =>
                  parent.declarations.collect {
                    case sym: TermSymbol if !sym.isAbstractMember && matchSymbol(method, sym) =>
                      BinaryMethod(binaryClass, sym, MixinForwarder)
                  }
                )

        candidates.singleOrThrow(method)

  def findClass(cls: binary.ClassType, isExtensionMethod: Boolean = false): BinaryClassSymbol =
    val javaParts = cls.name.split('.')
    val packageNames = javaParts.dropRight(1).toList.map(SimpleName.apply)
    val packageSym =
      if packageNames.nonEmpty
      then ctx.findSymbolFromRoot(packageNames).asInstanceOf[PackageSymbol]
      else ctx.defn.EmptyPackage
    val decodedClassName = NameTransformer.decode(javaParts.last)
    val allSymbols = decodedClassName match
      case Patterns.AnonClass(declaringClassName, remaining) =>
        val WithLocalPart = "(.+)\\$(.+)\\$\\d+".r
        val decl = declaringClassName match
          case WithLocalPart(decl, _) => decl.stripSuffix("$")
          case decl => decl
        findLocalClasses(cls, packageSym, decl, "$anon", remaining)
      case Patterns.LocalClass(declaringClassName, localClassName, remaining) =>
        findLocalClasses(cls, packageSym, declaringClassName, localClassName, remaining)
      case _ => findClassRecursively(packageSym, decodedClassName)
    if cls.isObject && !isExtensionMethod
    then allSymbols.filter(_.symbol.isModuleClass).singleOrThrow(cls)
    else allSymbols.filter(!_.symbol.isModuleClass).singleOrThrow(cls)

  private def notFound(symbol: binary.Symbol): Nothing = throw NotFoundException(symbol)

  private def withCompanionIfExtendsAnyVal(cls: ClassSymbol): Seq[ClassSymbol] =
    cls.companionClass match
      case Some(companionClass) if companionClass.isSubclass(ctx.defn.AnyValClass) =>
        Seq(cls, companionClass)
      case _ => Seq(cls)

  private def findLocalClasses(
      javaClass: binary.ClassType,
      packageSym: PackageSymbol,
      declaringClassName: String,
      localClassName: String,
      remaining: Option[String]
  ): Seq[BinaryClassSymbol] =
    val classOwners = findClassRecursively(packageSym, declaringClassName).map(_.symbol)
    remaining match
      case None =>
        val parents = (javaClass.superclass.toSet ++ javaClass.interfaces)
          .map(findClass(_))
          .collect { case BinaryClass(sym, _) => sym }
        val sourceLines = removeInlinedLines(javaClass.sourceLines, classOwners)
        if javaClass.sourceLines.isEmpty || sourceLines.nonEmpty then
          classOwners
            .flatMap(cls => collectLocalClasses(cls, localClassName, sourceLines))
            .collect {
              case cls: BinaryClass if matchParents(cls.symbol, parents, javaClass.isInterface) => cls
              case samCls: BinarySAMClass if matchSamClass(samCls.samClassSymbol, parents) => samCls
            }
        else Seq.empty
      case Some(remaining) =>
        val localClasses = classOwners
          .flatMap(cls => collectLocalClasses(cls, localClassName, Seq.empty))
          .collect { case BinaryClass(cls, _) => cls }
        localClasses.flatMap(s => findClassRecursively(s, remaining))

  private def findClassRecursively(owner: DeclaringSymbol, decodedName: String): Seq[BinaryClass] =
    owner.declarations
      .collect { case sym: ClassSymbol => sym }
      .flatMap { sym =>
        val Symbol = s"${Regex.quote(sym.nameStr)}\\$$?(.*)".r
        decodedName match
          case Symbol(remaining) =>
            if remaining.isEmpty then Some(BinaryClass(sym, TopLevelOrInner))
            else findClassRecursively(sym, remaining)
          case _ => None
      }

  private def collectLocalMethods(binaryClass: BinaryClass, kind: BinaryMethodKind, sourceLines: Seq[binary.SourceLine])(
      symbolMatcher: PartialFunction[(Symbol, Option[Lambda]), TermSymbol]
  ): Seq[BinaryMethod] =
    for
      cls <- withCompanionIfExtendsAnyVal(binaryClass.symbol)
      term <- collectLocalSymbols(cls, sourceLines)(symbolMatcher)
    yield BinaryMethod(binaryClass, term, kind)

  private def collectLocalClasses(
      cls: ClassSymbol,
      name: String,
      lines: Seq[binary.SourceLine]
  ): Seq[BinaryClassSymbol] =
    collectLocalSymbols(cls, lines) {
      case (cls: ClassSymbol, None) if cls.matchName(name) =>
        val kind = if name == "$anon" then Anon else Local
        BinaryClass(cls, kind)
      case (sym: TermSymbol, Some(lambda)) =>
        BinarySAMClass(sym, lambda.samClassSymbol, lambda.tpe.asInstanceOf[Type])
    }

  private def collectLocalSymbols[S](cls: ClassSymbol, lines: Seq[binary.SourceLine])(
      symbolMatcher: PartialFunction[(Symbol, Option[Lambda]), S]
  ): Seq[S] =
    val span = if lines.size > 2 then Seq(lines.min, lines.max) else lines

    val collectors = Buffer.empty[Collector]
    var inlinedSymbols = Set.empty[Symbol]

    class Collector(inlined: Boolean = false) extends TreeTraverser:
      collectors += this
      private var buffer = Map.empty[Symbol, S]
      override def traverse(tree: Tree): Unit =
        if matchLines(tree) then
          tree match
            case ValDef(_, _, _, symbol) if symbol.isLocal && (symbol.isLazyVal || symbol.isModuleVal) =>
              matchSymbol(symbol, None)
            case DefDef(_, _, _, _, symbol) if symbol.isLocal =>
              matchSymbol(symbol, None)
            case ClassDef(_, _, symbol) if symbol.isLocal =>
              matchSymbol(symbol, None)
            case lambda: Lambda =>
              val sym = lambda.meth.asInstanceOf[TermReferenceTree].symbol
              matchSymbol(sym, Some(lambda))
            case tree: Ident if isInline(tree) && !inlinedSymbols.contains(tree.symbol) =>
              inlinedSymbols += tree.symbol
              val collector = new Collector(inlined = true)
              tree.symbol.tree.foreach(collector.traverse)
            case _ => ()
          super.traverse(tree)

      def collected: Seq[S] =
        if inlined || span.isEmpty then buffer.values.toSeq
        else
          buffer
            .filterNot((symbol, _) => buffer.keys.exists(other => symbol.pos.isEnclosing(other.pos)))
            .values
            .toSeq

      private def matchSymbol(symbol: Symbol, lambda: Option[Lambda]) =
        symbolMatcher.lift((symbol, lambda)) match
          case Some(extracted) => buffer += (symbol -> extracted)
          case None => ()

      private def matchLines(tree: Tree): Boolean =
        inlined
          || tree.pos.isUnknown
          || !tree.pos.hasLineColumnInformation
          || span.forall(tree.pos.containsLine)

      private def isInline(tree: Ident): Boolean =
        try tree.symbol.isTerm && tree.symbol.asTerm.isInline
        catch case NonFatal(e) => false
    end Collector

    val collector = Collector()
    for
      decl <- cls.declarations
      tree <- decl.tree.toSeq
    do collector.traverse(tree)

    collectors.toSeq.flatMap(_.collected)
  end collectLocalSymbols

  private def removeInlinedLines(
      sourceLines: Seq[binary.SourceLine],
      classOwners: Seq[ClassSymbol]
  ): Seq[binary.SourceLine] =
    val inlineSymbols = classOwners.flatMap(collectInlineSymbols)
    sourceLines.filter(line =>
      classOwners.exists(_.pos.containsLine(line) && !inlineSymbols.exists(_.pos.containsLine(line)))
    )

  private def collectInlineSymbols(cls: ClassSymbol): Seq[TermSymbol] =
    val buffer = Buffer.empty[TermSymbol]
    val collector = new TreeTraverser:
      override def traverse(tree: Tree): Unit =
        tree match
          case termDef: ValOrDefDef if termDef.symbol.isInline => buffer += termDef.symbol
          case _ => ()
        super.traverse(tree)
    cls.tree.foreach(collector.traverse)
    buffer.toSeq

  private def matchParents(classSymbol: ClassSymbol, expectedParents: Set[ClassSymbol], isInterface: Boolean): Boolean =
    if classSymbol.isEnum then expectedParents == classSymbol.parentClasses.toSet + ctx.defn.ProductClass
    else if isInterface then expectedParents == classSymbol.parentClasses.filter(_.isTrait).toSet
    else if classSymbol.isAnonClass then classSymbol.parentClasses.forall(expectedParents.contains)
    else expectedParents == classSymbol.parentClasses.toSet

  private def matchSamClass(samClass: ClassSymbol, expectedParents: Set[ClassSymbol]): Boolean =
    if samClass == defn.partialFunction then
      expectedParents.size == 2 &&
      expectedParents.exists(_ == defn.abstractPartialFunction) &&
      expectedParents.exists(_ == defn.serializable)
    else expectedParents.contains(samClass)

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
    else if method.isTraitStaticAccessor then encodedScalaName == expectedName.stripSuffix("$")
    else encodedScalaName == expectedName

  private def matchSignature(method: binary.Method, symbol: TermSymbol): Boolean =
    def parametersName(tpe: TypeOrMethodic): List[String] =
      tpe match
        case t: MethodType =>
          t.paramNames.map(_.toString()) ++ parametersName(t.resultType)
        case t: PolyType =>
          parametersName(t.resultType)
        case _ => List()

    def matchCapture(paramName: String): Boolean =
      val pattern = ".+\\$\\d+".r
      pattern.matches(
        paramName
      ) || (method.isExtensionMethod && paramName == "$this") || (method.isTraitStaticAccessor && paramName == "$this") || (method.isClassInitializer && paramName == "$outer")

    val paramNames: List[String] = parametersName(symbol.declaredType)
    val capturedParams = method.allParameters.dropRight(paramNames.size)
    val declaredParams = method.allParameters.drop(capturedParams.size)
    capturedParams.map(_.name).forall(matchCapture) &&
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

  private def skip(method: BinaryMethodSymbol): Boolean =
    method match
      case BinaryMethod(_, sym, Getter) => !sym.isLazyValInTrait
      case BinaryMethod(_, _, Setter) => true
      case BinaryMethod(_, _, MixinForwarder) => true
      case BinaryMethod(_, _, TraitStaticAccessor) => true
      case BinaryMethod(_, sym, _) => sym.isSynthetic || sym.isExport
      case _ => false
