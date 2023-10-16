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
import ch.epfl.scala.debugadapter.internal.binary.Instruction

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
      case BinaryMethod(_, _, MixinForwarder | TraitStaticAccessor) => None
      case binaryMethod => Some(formatter.format(binaryMethod))

  def formatClass(cls: binary.ClassType): String =
    formatter.format(findClass(cls))

  def findMethod(method: binary.Method): BinaryMethodSymbol =
    val binaryClass = findClass(method.declaringClass, method.isExtensionMethod)
    findMethod(binaryClass, method)

  def findMethod(binaryClass: BinaryClassSymbol, method: binary.Method): BinaryMethodSymbol =
    binaryClass match
      case samClass: BinarySAMClass =>
        findAnonOverride(samClass, method).getOrElse(notFound(method))
      case partialFun: BinaryPartialFunction => findAnonOverride(partialFun, method)
      case binaryClass: BinaryClass =>
        val candidates = method match
          case Patterns.LocalLazyInit(name, _) =>
            collectLocalMethods(binaryClass, LocalLazyInit, method) { (term, inlined) =>
              (term.isLazyVal || term.isModuleVal) && term.matchName(name)
            }
          case Patterns.AnonFun(prefix) => findAnonFun(binaryClass, method)
          case Patterns.AdaptedAnonFun(prefix) => findAdaptedAnonFun(binaryClass, method)
          case Patterns.SuperArg() => findSuperArgs(binaryClass, method)
          case Patterns.LiftedTree() => findLiftedTry(binaryClass, method)
          case Patterns.LocalMethod(name, _) =>
            val localMethods = collectLocalMethods(binaryClass, LocalDef, method) { (term, inlined) =>
              term.matchName(name) && matchSignature(method, term, isInlined = inlined)
            }
            val anonGetters = if name == "x" then findInstanceMethods(binaryClass, method) else Seq.empty
            localMethods ++ anonGetters
          case Patterns.LazyInit(name) =>
            binaryClass.symbol.declarations.collect {
              case t: TermSymbol if t.isLazyVal && t.matchName(name) => BinaryMethod(binaryClass, t, LazyInit)
            }
          case Patterns.StaticAccessor(_) =>
            binaryClass.symbol.declarations.collect {
              case sym: TermSymbol if matchSymbol(method, sym) => BinaryMethod(binaryClass, sym, TraitStaticAccessor)
            }
          case Patterns.Outer(_) =>
            def outerClass(sym: Symbol): ClassSymbol =
              if sym.owner.isClass then sym.owner.asClass
              else outerClass(sym.owner)
            val outer = binaryClass.symbol.owner.owner
            List(BinaryOuter(binaryClass, outerClass(binaryClass.symbol)))
          case _ if method.isBridge => findBridgeAndMixinForwarders(binaryClass, method)
          case _ => findInstanceMethods(binaryClass, method)

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

    if cls.isObject && !isExtensionMethod then allSymbols.filter(_.symbol.isModuleClass).singleOrThrow(cls)
    else if cls.sourceLines.isEmpty && allSymbols.forall(_.symbol.isModuleClass) then
      allSymbols.singleOrThrow(cls) match
        case BinaryClass(symbol, TopLevelOrInner) => BinaryClass(symbol, SyntheticCompanionClass)
        case _ => notFound(cls)
    else allSymbols.filter(!_.symbol.isModuleClass).singleOrThrow(cls)

  private def findInstanceMethods(binaryClass: BinaryClass, method: binary.Method): Seq[BinaryMethod] =
    val fromClass: Seq[BinaryMethod] = binaryClass.symbol.declarations
      .collect {
        case sym: TermSymbol if matchSymbol(method, sym) =>
          if method.name == "$init$" then BinaryMethod(binaryClass, sym, TraitConstructor)
          else if method.name == "<init>" then BinaryMethod(binaryClass, sym, Constructor)
          else if !sym.isMethod then BinaryMethod(binaryClass, sym, Getter)
          else if sym.isSetter then BinaryMethod(binaryClass, sym, Setter)
          else if method.name.contains("$default$") then BinaryMethod(binaryClass, sym, DefaultParameter)
          else BinaryMethod(binaryClass, sym, InstanceDef)
      }
    if fromClass.nonEmpty then fromClass else findMethodsFromTraits(binaryClass, method)

  private def findBridgeAndMixinForwarders(binaryClass: BinaryClass, method: binary.Method): Seq[BinaryMethodSymbol] =
    val bridges =
      for
        overridingTerm <- binaryClass.symbol.declarations
          .collect { case term: TermSymbol if matchTargetName(method, term) => term }
        overriddenTerm <- overridingTerm.allOverriddenSymbols.find(matchSignature(method, _))
      yield
        val tpe = overriddenTerm.declaredTypeAsSeenFrom(binaryClass.symbol.thisType)
        val target = BinaryMethod(binaryClass, overridingTerm, BinaryMethodKind.InstanceDef)
        BinaryMethodBridge(target, tpe)
    if bridges.nonEmpty then bridges else findMethodsFromTraits(binaryClass, method)

  private def findAnonOverride(binaryClass: BinarySAMClass, method: binary.Method): Option[BinaryMethodSymbol] =
    val types =
      for
        parentCls <- binaryClass.parentClass.linearization.iterator
        overridden <- parentCls.declarations.collect { case term: TermSymbol if matchTargetName(method, term) => term }
        if overridden.overridingSymbol(binaryClass.parentClass).exists(_.isAbstractMember)
      yield
        val anonOverride = BinaryAnonOverride(binaryClass, overridden, binaryClass.term.declaredType)
        if method.isBridge then
          val bridgeTpe = overridden.declaredTypeAsSeenFrom(SkolemType(binaryClass.tpe))
          BinaryMethodBridge(anonOverride, bridgeTpe)
        else anonOverride
    types.nextOption

  private def findAnonOverride(binaryClass: BinaryPartialFunction, method: binary.Method): BinaryMethodSymbol =
    val overriddenMethod = defn.partialFunction.findNonOverloadedDecl(SimpleName(method.name))
    val tpe = overriddenMethod.declaredTypeAsSeenFrom(SkolemType(binaryClass.tpe))
    val anonOverride = BinaryAnonOverride(binaryClass, overriddenMethod, tpe)
    if method.isBridge then BinaryMethodBridge(anonOverride, tpe)
    else anonOverride

  private def findMethodsFromTraits(binaryClass: BinaryClass, method: binary.Method): Seq[BinaryMethod] =
    def allTraits(cls: ClassSymbol): Seq[ClassSymbol] =
      (cls.parentClasses ++ cls.parentClasses.flatMap(allTraits)).distinct.filter(_.isTrait)
    allTraits(binaryClass.symbol)
      .flatMap(_.declarations)
      .collect { case sym: TermSymbol if matchSymbol(method, sym) => sym }
      .collect {
        case sym if sym.isSetter => BinaryMethod(binaryClass, sym, TraitParamSetter)
        case sym if !sym.isMethod && sym.isParamAccessor => BinaryMethod(binaryClass, sym, TraitParamGetter)
        case sym if !sym.isAbstractMember => BinaryMethod(binaryClass, sym, MixinForwarder)
      }

  private def notFound(symbol: binary.Symbol): Nothing = throw NotFoundException(symbol)

  private def unexpected(message: String): Nothing = throw UnexpectedException(message)

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
              case samClass: BinarySAMClass if parents.contains(samClass.parentClass) => samClass
              case fun: BinaryPartialFunction if parents == Set(defn.abstractPartialFunction, defn.serializable) => fun
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

  private def collectLocalClasses(
      cls: ClassSymbol,
      name: String,
      lines: Seq[binary.SourceLine]
  ): Seq[BinaryClassSymbol] =
    object SAMClassOrPartialFunction:
      def unapply(lambda: Lambda): Option[BinaryPartialFunction | BinarySAMClass] =
        (lambda.meth.symbol, lambda.tpe) match
          case (term: TermSymbol, tpe: Type) =>
            if lambda.samClassSymbol == defn.partialFunction then Some(BinaryPartialFunction(term, tpe))
            else Some(BinarySAMClass(term, lambda.samClassSymbol, tpe))
          case _ => None
    collectTrees(cls, lines) {
      case ClassDef(_, _, cls) if cls.isLocal && cls.matchName(name) =>
        val kind = if name == "$anon" then Anon else Local
        BinaryClass(cls, kind)
      case SAMClassOrPartialFunction(binaryCls) => binaryCls
    }

  private def findAdaptedAnonFun(binaryClass: BinaryClass, method: binary.Method): Seq[BinaryMethodSymbol] =
    if method.instructions.nonEmpty then
      val underlying = method.instructions
        .collect {
          case Instruction.Method(_, owner, name, descriptor, _) if owner == method.declaringClass.name =>
            method.declaringClass.declaredMethod(name, descriptor)
        }
        .flatten
        .singleOrElse(unexpected(s"$method is not an adapted method: cannot find unerlying invocation"))
      findAnonFun(binaryClass, underlying).map {
        _ match
          case BinaryMethod(owner, term, AnonFun) => BinaryMethod(owner, term, AdaptedAnonFun)
          case BinaryByNameArg(owner, tpe, false) => BinaryByNameArg(owner, tpe, true)
          case _ => unexpected(s"Found invalid underlying method for $method: $underlying")
      }
    else findAnonFun(binaryClass, method, isAdapted = true)

  private def findAnonFun(
      binaryOwner: BinaryClass,
      method: binary.Method,
      isAdapted: Boolean = false
  ): Seq[BinaryMethodSymbol] =
    val kind = if isAdapted then AdaptedAnonFun else AnonFun
    val anonFuns = collectLocalMethods(binaryOwner, kind, method) { (term, inlined) =>
      term.isAnonFun && matchSignature(method, term, isAdapted = isAdapted, isInlined = inlined)
    }
    val byNameArgs =
      if method.allParameters.forall(_.isCapture) then findByNameArgs(binaryOwner, method, isAdapted)
      else Seq.empty
    anonFuns ++ byNameArgs

  private def findByNameArgs(
      binaryOwner: BinaryClass,
      method: binary.Method,
      isAdapted: Boolean
  ): Seq[BinaryByNameArg] =
    def matchType0(tpe: TermType): Boolean =
      (tpe, isAdapted) match
        case (tpe: Type, false) => method.returnType.exists(matchType(tpe.asErasedReturnType, _))
        case (tpe: Type, true) => tpe.asErasedReturnType.toString == "void"
        case _ => false
    val classOwners = withCompanionIfExtendsAnyVal(binaryOwner.symbol)
    val sourceSpan = removeInlinedLines(method.sourceLines, classOwners).interval
    object ByNameApply:
      def unapply(tree: Apply): Option[Seq[(TermTree, Type)]] =
        tree.fun.tpe.widenTermRef match
          case m: MethodType if m.paramTypes.size == tree.args.size =>
            Some(tree.args.zip(m.paramTypes).collect { case (arg, t: ByNameType) => (arg, t.resultType) })
              .filter(_.nonEmpty)
          case _ => None
    for
      classOwner <- classOwners
      byNameArg <- collectTrees1[Seq[BinaryByNameArg]](classOwner, sourceSpan)(inlined => { case ByNameApply(args) =>
        args.collect {
          case (arg, tpe) if matchType0(tpe) && (inlined || matchLines(arg, sourceSpan)) =>
            BinaryByNameArg(binaryOwner, tpe, isAdapted)
        }
      }).flatten
    yield byNameArg

  private def collectLocalMethods(
      binaryClass: BinaryClass,
      kind: BinaryMethodKind,
      javaMethod: binary.Method
  )(
      predicate: (TermSymbol, Boolean) => Boolean
  ): Seq[BinaryMethod] =
    val classOwners = withCompanionIfExtendsAnyVal(binaryClass.symbol)
    val sourceLines = removeInlinedLines(javaMethod.sourceLines, classOwners)
    for
      cls <- classOwners
      term <- collectTrees1(cls, sourceLines) { inlined =>
        val treeMatcher: PartialFunction[Tree, TermSymbol] = {
          case ValDef(_, _, _, sym) if sym.isLocal && (sym.isLazyVal || sym.isModuleVal) => sym
          case DefDef(_, _, _, _, sym) if sym.isLocal => sym
        }
        treeMatcher.andThen { case sym if predicate(sym, inlined) => sym }
      }
    yield BinaryMethod(binaryClass, term, kind)

  private def findSuperArgs(binaryOwner: BinaryClass, method: binary.Method): Seq[BinarySuperArg] =
    def asSuperCall(parent: Tree): Option[Apply] =
      parent match
        case superCall: Apply => Some(superCall)
        case block: Block => asSuperCall(block.expr)
        case _ => None

    def asType(tpe: TermType): Option[Type] =
      tpe match
        case tpe: Type => Some(tpe)
        case _ => None

    def asSuperCons(fun: TermTree): Option[TermSymbol] =
      fun match
        case Apply(fun, _) => asSuperCons(fun)
        case s @ Select(_: New, _) if s.symbol.isTerm => Some(s.symbol.asTerm)
        case _ => None

    def toFunction0(argType: Type): Type =
      AppliedType(TypeRef(defn.scalaPackage.packageRef, ctx.defn.Function0Class), List(argType))

    val span = method.sourceLines.interval
    val localClasses = collectTrees(binaryOwner.symbol, span) { case ClassDef(_, _, cls) if cls.isLocal => cls }
    val innerClasses = binaryOwner.symbol.declarations.collect {
      case cls: ClassSymbol if cls.pos.unknownOrContainsAll(span) => cls
    }
    for
      cls <- binaryOwner.symbol +: (localClasses ++ innerClasses)
      tree <- cls.tree.toSeq
      init = tree.rhs.constr.symbol
      parent <- tree.rhs.parents
      superCall <- asSuperCall(parent).toSeq
      superCons <- asSuperCons(superCall.fun).toSeq
      paramTypes = superCons.declaredType.allParamsTypes
      args = superCall.allArgsFlatten
      if args.size == paramTypes.size
      (arg, paramType) <- args.zip(paramTypes)
      if arg.pos.unknownOrContainsAll(span)
      argType0 <- asType(arg.tpe).toSeq
      argType = paramType match
        case byName: ByNameType =>
          byName.resultType.widen match
            case tpe: Type => toFunction0(tpe)
            case _ => toFunction0(byName.resultType)
        case _ => argType0
      if method.returnType.forall(matchType(argType.asErasedArgType, _))
    yield BinarySuperArg(binaryOwner, init, argType)
  end findSuperArgs

  private def findLiftedTry(binaryOwner: BinaryClass, method: binary.Method): Seq[BinaryLiftedTry] =
    def matchType0(tpe: TermType): Boolean =
      tpe match
        case tpe: Type => method.returnType.exists(matchType(tpe.asErasedReturnType, _))
        case _ => false
    val classOwners = withCompanionIfExtendsAnyVal(binaryOwner.symbol)
    val sourceSpan = removeInlinedLines(method.sourceLines, classOwners).interval
    for
      classOwner <- classOwners
      liftedTry <- collectTrees[BinaryLiftedTry](classOwner, sourceSpan) {
        case tryTree: Try if matchType0(tryTree.tpe) =>
          BinaryLiftedTry(binaryOwner, tryTree.tpe.asInstanceOf[Type])
      }
    yield liftedTry

  private def collectTrees[S](cls: Symbol, lines: Seq[binary.SourceLine])(matcher: PartialFunction[Tree, S]): Seq[S] =
    collectTrees1(cls, lines)(_ => matcher)

  private def collectTrees1[S](root: Symbol, lines: Seq[binary.SourceLine])(
      matcher: Boolean => PartialFunction[Tree, S]
  ): Seq[S] =
    val span = lines.interval
    val collectors = Buffer.empty[Collector]
    var inlinedSymbols = Set.empty[Symbol]

    object InlinedTree:
      def unapply(tree: Tree): Option[Symbol] =
        tree match
          case tree: TermReferenceTree if tree.symbol.isInline => Some(tree.symbol)
          case Apply(fun, _) => unapply(fun)
          case TypeApply(fun, _) => unapply(fun)
          case _ => None

    class Collector(inlined: Boolean = false) extends TreeTraverser:
      collectors += this
      private var buffer = Map.empty[Tree, S]
      override def traverse(tree: Tree): Unit =
        if inlined || matchLines(tree, span) then
          tree match
            case InlinedTree(symbol) if !inlinedSymbols.contains(symbol) =>
              inlinedSymbols += symbol
              val collector = new Collector(inlined = true)
              symbol.tree.foreach(collector.traverse)
            case tree => matchTree(tree)
          super.traverse(tree)
        else
          // bug in dotty: wrong pos of `def $new` in the companion object of an enum
          // the pos is outside the companion object, in the enum
          tree match
            case ClassDef(_, template, sym) if sym.companionClass.exists(_.isEnum) =>
              super.traverse(template.body)
            case _ => ()

      def collected: Seq[S] =
        if inlined || lines.isEmpty then buffer.values.toSeq
        else
          buffer
            .filterNot((tree, _) => buffer.keys.exists(other => tree.pos.isEnclosing(other.pos)))
            .values
            .toSeq

      private def matchTree(tree: Tree): Unit =
        matcher(inlined).lift(tree).foreach(res => buffer += (tree -> res))
    end Collector

    val collector = Collector()
    root.tree.foreach(collector.traverse)
    collectors.toSeq.flatMap(_.collected)
  end collectTrees1

  private def matchLines(tree: Tree, span: Seq[binary.SourceLine]): Boolean =
    tree match
      case lambda: Lambda => lambda.meth.symbol.tree.exists(matchLines(_, span))
      case tree => tree.pos.unknownOrContainsAll(span)

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

  private def matchSymbol(method: binary.Method, symbol: TermSymbol): Boolean =
    matchTargetName(method, symbol) && (method.isTraitInitializer || matchSignature(method, symbol))

  private def matchTargetName(method: binary.Method, symbol: TermSymbol): Boolean =
    val symbolName = symbol.targetName.toString
    val scalaName = symbol.targetName.toString match
      case "<init>" if symbol.owner.asClass.isTrait => "$init$"
      case "<init>" => "<init>"
      case scalaName => scalaName
    if method.isExtensionMethod then scalaName == method.unexpandedDecodedName.stripSuffix("$extension")
    else if method.isTraitStaticAccessor then scalaName == method.unexpandedDecodedName.stripSuffix("$")
    else scalaName == method.unexpandedDecodedName

  private def matchSignature(
      method: binary.Method,
      symbol: TermSymbol,
      isAdapted: Boolean = false,
      isInlined: Boolean = false
  ): Boolean =
    object CurriedContextFunction:
      def unapply(tpe: Type): Option[(Seq[TypeOrWildcard], TypeOrWildcard)] =
        def rec(tpe: TypeOrWildcard, args: Seq[TypeOrWildcard]): Option[(Seq[TypeOrWildcard], TypeOrWildcard)] =
          tpe match
            case tpe: AppliedType if tpe.tycon.isContextFunction => rec(tpe.args.last, args ++ tpe.args.init)
            case res => Option.when(args.nonEmpty)((args, res))
        rec(tpe, Seq.empty)

    val paramsSig = symbol.signature.paramsSig.collect { case ParamSig.Term(sig) => sig }
    val resSig = symbol.signature.resSig
    val paramNames = symbol.declaredType.allParamsNames.map(_.toString)
    symbol.declaredType.returnType.dealias match
      case CurriedContextFunction(uncurriedArgs, uncurriedReturnType) if !symbol.isAnonFun =>
        val capturedParams = method.allParameters.dropRight(paramNames.size + uncurriedArgs.size)
        val declaredParams = method.allParameters.drop(capturedParams.size).dropRight(uncurriedArgs.size)
        val contextParams = method.allParameters.drop(capturedParams.size + declaredParams.size)

        (capturedParams ++ contextParams).forall(_.isGenerated) &&
        declaredParams.map(_.name).corresponds(paramNames)(_ == _) &&
        (isAdapted || isInlined || matchSignature(
          paramsSig ++ uncurriedArgs.map(_.asErasedArgType),
          uncurriedReturnType.asErasedReturnType,
          declaredParams ++ contextParams,
          method.returnType
        ))
      case _ =>
        val capturedParams = method.allParameters.dropRight(paramNames.size)
        val declaredParams = method.allParameters.drop(capturedParams.size)

        capturedParams.forall(_.isGenerated) &&
        declaredParams.map(_.name).corresponds(paramNames)(_ == _) &&
        (isAdapted || isInlined || matchSignature(paramsSig, resSig, declaredParams, method.returnType))

  private def matchSignature(
      scalaParams: Seq[FullyQualifiedName],
      scalaReturnType: FullyQualifiedName,
      javaParams: Seq[binary.Parameter],
      javaReturnType: Option[binary.Type]
  ): Boolean =
    scalaParams.corresponds(javaParams)((scalaParam, javaParam) => matchType(scalaParam, javaParam.`type`)) &&
      javaReturnType.forall(matchType(scalaReturnType, _))

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
      case BinaryMethod(_, sym, kind) =>
        kind match
          case Getter => !sym.isLazyValInTrait
          case Setter | MixinForwarder | TraitStaticAccessor | AdaptedAnonFun | TraitParamGetter | TraitParamSetter =>
            true
          case _ => sym.isSynthetic || sym.isExport
      case BinaryByNameArg(_, _, isAdapted) => isAdapted
      case BinaryMethodBridge(_, _) => true
      case _ => false
