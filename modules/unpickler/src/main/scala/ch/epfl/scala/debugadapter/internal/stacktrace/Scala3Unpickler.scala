package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.internal.binary
import ch.epfl.scala.debugadapter.internal.jdi.JdiMethod
import ch.epfl.scala.debugadapter.internal.stacktrace.*
import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Signatures.*
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.jdk.ClasspathLoaders

import java.nio.file.Path
import java.util.Optional
import java.util.function.Consumer
import scala.jdk.OptionConverters.*
import scala.util.matching.Regex
import tastyquery.SourceLanguage
import tastyquery.Traversers.TreeTraverser
import scala.collection.mutable.Buffer
import ch.epfl.scala.debugadapter.internal.binary.Instruction

class Scala3Unpickler(
    classpaths: Array[Path],
    binaryClassLoader: binary.BinaryClassLoader,
    warnLogger: Consumer[String],
    testMode: Boolean
) extends ThrowOrWarn(warnLogger.accept, testMode):
  private val classpath = ClasspathLoaders.read(classpaths.toList)
  private given ctx: Context = Context.initialize(classpath)
  private val defn = new Definitions
  private[stacktrace] val formatter = new Scala3Formatter(warnLogger.accept, testMode)

  def skipMethod(obj: Any): Boolean =
    skipMethod(JdiMethod(obj): binary.Method)

  def skipMethod(method: binary.Method): Boolean =
    // TODO remove try catch
    try
      val symbol = findMethod(method)
      skip(findMethod(method))
    catch case _ => true

  private[stacktrace] def skip(method: BinaryMethodSymbol): Boolean =
    method match
      case BinaryMethod(_, sym) =>
        (sym.isGetter && (!sym.owner.isTrait || !sym.isModuleOrLazyVal)) || // getter
        (sym.isLocal && sym.isModuleOrLazyVal) || // local def
        sym.isSetter ||
        (sym.isSynthetic && !sym.isLocal) ||
        sym.isExport
      case BinaryLazyInit(_, sym) => sym.owner.isTrait
      case _: BinaryTraitStaticForwarder => true
      case _: BinaryTraitParamAccessor => true
      case _: BinaryMixinForwarder => true
      case _: BinaryMethodBridge => true
      case _: BinaryStaticForwarder => true
      case _: BinaryOuter => true
      case _: BinarySetter => true
      case _: BinarySuperAccessor => true
      case _: BinarySpecializedMethod => true
      case _: BinaryInlineAccessor => true
      case _: BinaryAdaptedFun => true
      case _: BinarySAMClassConstructor => true
      case _ => false

  def formatMethod(obj: Any): Optional[String] =
    formatMethod(JdiMethod(obj)).toJava

  def formatMethod(method: binary.Method): Option[String] =
    val binaryMethod = findMethod(method)
    binaryMethod match
      case BinaryLazyInit(_, sym) if sym.owner.isTrait => None
      case _: BinaryMixinForwarder => None
      case _: BinaryTraitStaticForwarder => None
      case _: BinaryMethodBridge => None
      case _: BinaryStaticForwarder => None
      case BinarySetter(_, sym, _) if sym.isVal => None
      case _: BinarySuperAccessor => None
      case _: BinarySpecializedMethod => None
      case _: BinaryInlineAccessor => None
      case _: BinaryAdaptedFun => None
      case m => Some(formatter.format(m))

  def formatClass(cls: binary.ClassType): String =
    formatter.format(findClass(cls))

  def findMethod(method: binary.Method): BinaryMethodSymbol =
    val binaryClass = findClass(method.declaringClass)
    findMethod(binaryClass, method)

  def findMethod(binaryClass: BinaryClassSymbol, method: binary.Method): BinaryMethodSymbol =
    def requiresBinaryClass(f: BinaryClass => Seq[BinaryMethodSymbol]): Seq[BinaryMethodSymbol] =
      binaryClass match
        case bc: BinaryClass => f(bc)
        case _ => Seq.empty

    def find(f: PartialFunction[binary.Method, Seq[BinaryMethodSymbol]]): Seq[BinaryMethodSymbol] =
      f.applyOrElse(method, _ => Seq.empty[BinaryMethodSymbol])

    extension (xs: Seq[BinaryMethodSymbol])
      def orFind(f: PartialFunction[binary.Method, Seq[BinaryMethodSymbol]]): Seq[BinaryMethodSymbol] =
        if xs.nonEmpty then xs else f.applyOrElse(method, _ => Seq.empty[BinaryMethodSymbol])
    val candidates =
      find { case Patterns.LocalLazyInit(names) =>
        requiresBinaryClass(collectLocalMethods(_, method)(inlined => {
          case term if (term.isLazyVal || term.isModuleVal) && names.contains(term.nameStr) =>
            BinaryLocalLazyInit(binaryClass, term)
        }))
      }
        .orFind { case Patterns.AnonFun(_) => findAnonFunAndByNameArgs(binaryClass, method) }
        .orFind { case Patterns.AdaptedAnonFun(_) => findAdaptedAnonFun(binaryClass, method) }
        .orFind { case Patterns.ByNameArgProxy() => findByNameArgsProxy(binaryClass, method) }
        .orFind { case Patterns.SuperArg() => requiresBinaryClass(findSuperArgs(_, method)) }
        .orFind { case Patterns.LiftedTree() => findLiftedTry(binaryClass, method) }
        .orFind { case Patterns.LocalMethod(names) =>
          val localMethods = collectLocalMethods(binaryClass, method)(inlined => {
            case term if names.contains(term.nameStr) && matchSignature(method, term, checkTypeErasure = !inlined) =>
              BinaryMethod(binaryClass, term)
          })
          val anonTraitParamGetters =
            if names.contains("x") then requiresBinaryClass(findInstanceMethods(_, method))
            else Seq.empty
          localMethods ++ anonTraitParamGetters
        }
        .orFind { case Patterns.LazyInit(name) => requiresBinaryClass(findLazyInit(_, name)) }
        .orFind { case Patterns.Outer(_) => Seq(findOuter(binaryClass)) }
        .orFind { case Patterns.TraitInitializer() => requiresBinaryClass(findTraitInitializer(_, method)) }
        .orFind { case Patterns.InlineAccessor(names) =>
          names.flatMap(findInlineAccessorOrForwarder(binaryClass, method, _))
        }
        .orFind { case Patterns.ValueClassExtension() =>
          if method.isStatic then requiresBinaryClass(findValueClassForwarders(_, method))
          else requiresBinaryClass(findValueClassExtension(_, method))
        }
        .orFind { case Patterns.DeserializeLambda() =>
          Seq(BinaryDeserializeLambda(binaryClass, defn.DeserializeLambdaType))
        }
        .orFind { case Patterns.ParamForwarder(name) => requiresBinaryClass(findParamForwarder(_, method, name)) }
        .orFind { case Patterns.TraitSetter(name) =>
          if method.isStatic then findTraitSetterForwarder(binaryClass, method, name)
          else requiresBinaryClass(findTraitSetter(_, method, name))
        }
        .orFind { case Patterns.Setter(names) =>
          findStandardMethod(binaryClass, method).orIfEmpty(requiresBinaryClass(findSetter(_, method, names)))
        }
        .orFind { case Patterns.SuperAccessor(names) => requiresBinaryClass(findSuperAccessor(_, method, names)) }
        .orFind { case Patterns.SpecializedMethod(names) =>
          if method.isStatic then findSpecializedForwarder(binaryClass, method, names)
          else requiresBinaryClass(findSpecializedMethod(_, method, names))
        }
        .orFind { case Patterns.TraitStaticForwarder(names) =>
          requiresBinaryClass(findTraitStaticForwarder(_, method, names))
        }
        .orFind {
          case _ if method.isStatic =>
            // todo: this should be the only way to create a BinaryStaticForwarder
            findStaticForwarder(binaryClass, method)
        }
        .orFind { case _ => findStandardMethod(binaryClass, method) }
        .orFind { case _ if method.isStatic && binaryClass.isJava => throw IgnoredException(method, "Java static method") }
    candidates.singleOrThrow(method)
  end findMethod

  def findClass(cls: binary.ClassType): BinaryClassSymbol =
    val javaParts = cls.name.split('.')
    val packageNames = javaParts.dropRight(1).toList.map(SimpleName.apply)
    val packageSym =
      if packageNames.nonEmpty
      then ctx.findSymbolFromRoot(packageNames).asInstanceOf[PackageSymbol]
      else defn.EmptyPackage
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

    val candidates =
      if cls.isObject then allSymbols.filter(_.symbol.isModuleClass)
      else if cls.sourceLines.isEmpty && allSymbols.forall(_.symbol.isModuleClass) then
        allSymbols.collect { case BinaryClass(symbol) => BinarySyntheticCompanionClass(symbol) }
      else allSymbols.filter(!_.symbol.isModuleClass)
    candidates.singleOrThrow(cls)

  private def findStandardMethod(binaryClass: BinaryClassSymbol, method: binary.Method): Seq[BinaryMethodSymbol] =
    binaryClass match
      case samClass: BinarySAMClass =>
        if method.isBridge then
          findBridgesAndMixinForwarders(samClass, samClass.parentClass, samClass.declaredType, method)
        else if method.isConstructor then Seq(BinarySAMClassConstructor(samClass, samClass.declaredType))
        else findAnonOverride(samClass, method).toSeq
      case partialFun: BinaryPartialFunction =>
        if method.isConstructor then Seq(BinarySAMClassConstructor(partialFun, partialFun.declaredType))
        else Seq(findAnonOverride(partialFun, method))
      case binaryClass: BinaryClass =>
        if method.isBridge then findBridgesAndMixinForwarders(binaryClass, method)
        else findInstanceMethods(binaryClass, method)
      case syntheticClass: BinarySyntheticCompanionClass => Seq.empty

  private def findParamForwarder(binaryClass: BinaryClass, method: binary.Method, name: String): Seq[BinaryMethod] =
    binaryClass.symbol.declarations.collect {
      case sym: TermSymbol if sym.targetNameStr == name && matchSignature(method, sym) =>
        BinaryMethod(binaryClass, sym)
    }

  private def findTraitSetter(binaryClass: BinaryClass, method: binary.Method, name: String): Seq[BinarySetter] =
    for
      traitSym <- binaryClass.symbol.linearization.filter(_.isTrait)
      sym <- traitSym.declarations.collect {
        case sym: TermSymbol if sym.targetNameStr == name && !sym.isMethod && !sym.isAbstractMember => sym
      }
      paramType <- sym.declaredTypeAsSeenFrom(binaryClass.symbol.thisType) match
        case tpe: Type => Some(tpe)
        case _ => None
    yield
      val tpe = MethodType(List(SimpleName("x$1")), List(paramType), defn.UnitType)
      BinarySetter(binaryClass, sym, tpe)

  private def findTraitSetterForwarder(
      binaryClass: BinaryClassSymbol,
      method: binary.Method,
      name: String
  ): Seq[BinaryStaticForwarder] =
    for
      binaryObject <- binaryClass.companionClass.toSeq
      target <- findTraitSetter(binaryObject, method, name)
    yield BinaryStaticForwarder(binaryClass, target, target.declaredType)

  private def findSetter(binaryClass: BinaryClass, method: binary.Method, names: Seq[String]): Seq[BinarySetter] =
    val javaParamType = method.allParameters.last.`type`
    def matchType0(sym: TermSymbol): Boolean =
      sym.declaredType match
        case tpe: Type => matchType(tpe.erasedAsArgType(), javaParamType)
        case _ => false
    binaryClass.symbol.declarations.collect {
      case sym: TermSymbol if !sym.isMethod && names.exists(sym.targetNameStr == _) && matchType0(sym) =>
        val tpe = MethodType(List(SimpleName("x$1")), List(sym.declaredType.asInstanceOf[Type]), defn.UnitType)
        BinarySetter(binaryClass, sym, tpe)
    }

  private def findSuperAccessor(
      binaryClass: BinaryClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[BinaryMethodSymbol] =
    for
      traitSym <- binaryClass.symbol.linearization.filter(_.isTrait)
      sym <- traitSym.declarations.collect {
        case sym: TermSymbol if names.exists(_ == sym.targetNameStr) && !sym.isAbstractMember => sym
      }
      expectedTpe =
        if method.isBridge then sym.declaredType
        else sym.declaredTypeAsSeenFrom(binaryClass.symbol.thisType)
      if sym.isOverridingSymbol(binaryClass.symbol) &&
        matchSignature1(method, expectedTpe, isAnonFun = false)
    yield
      val tpe = sym.declaredTypeAsSeenFrom(binaryClass.symbol.thisType)
      val accessor = BinarySuperAccessor(binaryClass, sym, tpe)
      if method.isBridge then BinaryMethodBridge(accessor, tpe)
      else accessor

  private def findSpecializedMethod(
      binaryClass: BinaryClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[BinarySpecializedMethod] =
    binaryClass.symbol.declarations.collect {
      case sym: TermSymbol
          if names.exists(_ == sym.targetNameStr) &&
            matchSignature(method, sym, checkParamNames = false, checkTypeErasure = false) &&
            // hack: in Scala 3 only overriding symbols can be specialized (Function and Tuple)
            sym.allOverriddenSymbols.nonEmpty =>
        BinarySpecializedMethod(binaryClass, sym)
    }

  private def findInlineAccessorOrForwarder(
      binaryClass: BinaryClassSymbol,
      method: binary.Method,
      name: String
  ): Seq[BinaryMethodSymbol] =
    def requiresBinaryClass(f: BinaryClass => Seq[BinaryMethodSymbol]): Seq[BinaryMethodSymbol] =
      binaryClass match
        case bc: BinaryClass => f(bc)
        case _ => Seq.empty
    if method.isTraitStaticForwarder then
      requiresBinaryClass(findInlineAccessor(_, method, name.stripSuffix("$"))).map(BinaryTraitStaticForwarder.apply)
    else if method.isStatic then
      for
        binaryObject <- binaryClass.companionClass.toSeq
        target <- findInlineAccessor(binaryObject, method, name, checkParamNames = false)
      yield BinaryStaticForwarder(binaryClass, target, target.declaredType)
    else
      def mixinForwarder(binaryClass: BinaryClass) =
        if !binaryClass.symbol.isTrait then
          for
            interface <- allInterfaces(method.declaringClass).filter(_.declaredMethods.exists(_.name == method.name))
            traitSym <- findClass(interface) match
              case BinaryClass(sym) if sym.isTrait => Seq(sym)
              case _ => Seq.empty
            inlineAccessor <- findInlineAccessor(BinaryClass(traitSym), method, name)
          yield BinaryMixinForwarder(binaryClass, inlineAccessor)
        else Seq.empty
      requiresBinaryClass(bc => mixinForwarder(bc).orIfEmpty(findInlineAccessor(bc, method, name)))

  def allInterfaces(cls: binary.ClassType): Seq[binary.ClassType] =
    def rec(cls: binary.ClassType, acc: Seq[binary.ClassType]): Seq[binary.ClassType] =
      val newInterfaces = cls.interfaces.filter(!acc.contains(_))
      (newInterfaces ++ cls.superclass).foldLeft(acc ++ newInterfaces)((acc, cls) => rec(cls, acc))
    rec(cls, Seq.empty)

  private def findInlineAccessor(
      binaryClass: BinaryClass,
      method: binary.Method,
      name: String,
      checkParamNames: Boolean = true
  ): Seq[BinaryMethodSymbol] =
    val hasReceiver = "(.+)\\$i\\d+".r
    val inlineable = name match
      case hasReceiver(name) =>
        for
          receiverParam <- method.allParameters.filter(_.name != "$this").headOption.toSeq
          receiverClass <- receiverParam.`type` match
            case cls: binary.ClassType => Seq(findClass(cls)).collect { case cls: BinaryClass => cls }
            case _ => Seq.empty
          inlineable <- findInlineableMethod(receiverClass, method, name, checkParamNames)
        yield inlineable
      case s"${name}$$extension" =>
        binaryClass.companionClass.toSeq.flatMap(findInlineableMethod(_, method, name, checkParamNames))
      case _ => findInlineableMethod(binaryClass, method, name, checkParamNames)
    inlineable.map(BinaryInlineAccessor(binaryClass, _))

  private def findInlineableMethod(
      binaryClass: BinaryClass,
      method: binary.Method,
      name: String,
      checkParamNames: Boolean = true
  ): Seq[BinaryMethodSymbol] =
    val standardMethods =
      for
        classSym <- binaryClass.symbol.linearization
        sym <- classSym.declarations.collect { case sym: TermSymbol if sym.targetNameStr == name => sym }
        if sym.isOverridingSymbol(binaryClass.symbol)
        // should I compute the declaredType type as seen from binaryClass.symbol?
        resultType = sym.declaredType match
          case byName: ByNameType => byName.resultType
          case tpe => tpe
        if matchSignature1(method, resultType, isAnonFun = false, checkParamNames = checkParamNames)
      yield BinaryMethod(BinaryClass(classSym), sym)
    def setters =
      name match
        case s"${name}_=" =>
          val javaParamType = method.allParameters.last.`type`
          for
            classSym <- binaryClass.symbol.linearization
            sym <- classSym.declarations.collect {
              case sym: TermSymbol if !sym.isMethod && sym.targetNameStr == name => sym
            }
            if sym.isOverridingSymbol(binaryClass.symbol)
            // should I compute the declaredType type as seen from binaryClass.symbol?
            if sym.declaredType match
              case tpe: Type => matchType(tpe.erasedAsArgType(), javaParamType)
              case _ => false
          yield
            val declaredTpe =
              MethodType(List(SimpleName("x$1")), List(sym.declaredType.asInstanceOf[Type]), defn.UnitType)
            BinarySetter(BinaryClass(classSym), sym, declaredTpe)
        case _ => Seq.empty
    standardMethods.orIfEmpty(setters)

  private def findSpecializedForwarder(
      binaryClass: BinaryClassSymbol,
      method: binary.Method,
      names: Seq[String]
  ): Seq[BinaryStaticForwarder] =
    for
      binaryObject <- binaryClass.companionClass.toSeq
      companionObject <- binaryObject.classSymbol.toSeq
      cls <- companionObject.linearization
      sym <- cls.declarations.collect {
        case sym: TermSymbol
            if names.exists(sym.targetNameStr == _) &&
              matchSignature(method, sym, checkParamNames = false, checkTypeErasure = false) &&
              // hack: in Scala 3 only overriding symbols can be specialized (Function and Tuple)
              sym.allOverriddenSymbols.nonEmpty =>
          sym
      }
      if sym.isOverridingSymbol(companionObject)
    yield
      val target = BinarySpecializedMethod(BinaryClass(companionObject), sym)
      BinaryStaticForwarder(binaryClass, target, sym.declaredTypeAsSeenFrom(companionObject.thisType))

  private def findInstanceMethods(binaryClass: BinaryClass, method: binary.Method): Seq[BinaryMethodSymbol] =
    if method.isConstructor && binaryClass.symbol.isSubClass(defn.AnyValClass) then
      binaryClass.symbol.getAllOverloadedDecls(SimpleName("<init>")).map(BinaryMethod(binaryClass, _))
    else
      val fromClass = binaryClass.symbol.declarations
        .collect { case sym: TermSymbol if matchTargetName(method, sym) => sym }
        .collect {
          case sym if matchSignature(method, sym, checkParamNames = !binaryClass.isJava) => BinaryMethod(binaryClass, sym)
          case sym if matchSignature(method, sym, asJavaVarargs = true, checkParamNames = !binaryClass.isJava) =>
            val method = BinaryMethod(binaryClass, sym)
            if binaryClass.isJava then method else BinaryMethodBridge(method, sym.declaredType)
        }
      // TODO: can a mixin forwarder not be a bridge? (yes if it's a getter of a lazy val, some other cases?)
      // Can a trait param accessor be a bridge?
      // figure out and split findMethodsFromTraits in 2
      fromClass.orIfEmpty(findBridgesAndMixinForwarders(binaryClass, method))

  private def findLazyInit(binaryClass: BinaryClass, name: String): Seq[BinaryMethodSymbol] =
    val matcher: PartialFunction[Symbol, TermSymbol] =
      case sym: TermSymbol if sym.isModuleOrLazyVal && sym.nameStr == name => sym
    val fromClass = binaryClass.symbol.declarations.collect(matcher).map(BinaryLazyInit(binaryClass, _))
    def fromTraits =
      for
        traitSym <- binaryClass.symbol.linearization.filter(_.isTrait)
        term <- traitSym.declarations.collect(matcher)
        if term.isOverridingSymbol(binaryClass.symbol)
      yield BinaryLazyInit(binaryClass, term)
    fromClass.orIfEmpty(fromTraits)

  private def findTraitStaticForwarder(
      binaryClass: BinaryClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[BinaryMethodSymbol] =
    binaryClass.symbol.declarations.collect {
      case sym: TermSymbol if names.contains(sym.targetNameStr) && matchSignature(method, sym) =>
        BinaryTraitStaticForwarder(BinaryMethod(binaryClass, sym))
    }

  private def findOuter(binaryClass: BinaryClassSymbol): BinaryOuter =
    def outerClass(sym: Symbol): ClassSymbol = if sym.isClass then sym.asClass else outerClass(sym.owner)
    val outerType = outerClass(binaryClass.symbol.owner).thisType
    BinaryOuter(binaryClass, outerType)

  private def findTraitInitializer(binaryClass: BinaryClass, method: binary.Method): Seq[BinaryMethod] =
    binaryClass.symbol.declarations.collect {
      case sym: TermSymbol if sym.nameStr == "<init>" => BinaryMethod(binaryClass, sym)
    }

  private def findValueClassExtension(binaryClass: BinaryClass, method: binary.Method): Seq[BinaryMethod] =
    val names = method.unexpandedDecodedNames.map(_.stripSuffix("$extension"))
    val companionClassOpt = binaryClass.symbol.companionClass
    companionClassOpt.toSeq.flatMap(_.declarations).collect {
      case sym: TermSymbol if names.contains(sym.targetNameStr) && matchSignature(method, sym) =>
        BinaryMethod(binaryClass, sym)
    }

  private def findValueClassForwarders(binaryClass: BinaryClass, method: binary.Method): Seq[BinaryStaticForwarder] =
    val names = method.unexpandedDecodedNames.map(_.stripSuffix("$extension"))
    for
      companionObject <- binaryClass.symbol.companionClass.toSeq
      sym <- binaryClass.symbol.declarations.collect {
        case sym: TermSymbol if names.contains(sym.targetNameStr) => sym
      }
      if matchSignature(method, sym, checkParamNames = false)
    yield
      val target = BinaryMethod(BinaryClass(companionObject), sym)
      BinaryStaticForwarder(binaryClass, target, sym.declaredType)

  private def findStaticForwarder(binaryClass: BinaryClassSymbol, method: binary.Method): Seq[BinaryStaticForwarder] =
    binaryClass.companionClass.toSeq.flatMap(findStaticForwarder(binaryClass, _, method))

  private def findStaticForwarder(
      binaryClass: BinaryClassSymbol,
      companionObject: BinaryClass,
      method: binary.Method
  ): Seq[BinaryStaticForwarder] =
    if method.instructions.nonEmpty then
      method.instructions
        .collect { case Instruction.Method(_, owner, name, descriptor, _) =>
          binaryClassLoader.loadClass(owner).method(name, descriptor)
        }
        .flatten
        .singleOpt
        .toSeq
        .map { binaryTarget =>
          val target = findMethod(binaryTarget)
          val declaredType = target.termSymbol
            .map(_.declaredTypeAsSeenFrom(companionObject.symbol.thisType))
            .getOrElse(target.declaredType)
          BinaryStaticForwarder(binaryClass, target, declaredType)
        }
    else
      for
        companionSym <- companionObject.classSymbol.toSeq
        cls <- companionSym.linearization
        sym <- cls.declarations.collect {
          case sym: TermSymbol
              if matchTargetName(method, sym) && matchSignature(method, sym, checkParamNames = false) =>
            sym
        }
        if sym.isOverridingSymbol(companionSym)
      yield
        val target = BinaryMethod(BinaryClass(cls), sym)
        BinaryStaticForwarder(binaryClass, target, sym.declaredTypeAsSeenFrom(companionSym.thisType))

  private def findAnonOverride(binaryClass: BinarySAMClass, method: binary.Method): Option[BinaryMethodSymbol] =
    val types =
      for
        parentCls <- binaryClass.parentClass.linearization.iterator
        overridden <- parentCls.declarations.collect { case term: TermSymbol if matchTargetName(method, term) => term }
        if overridden.overridingSymbol(binaryClass.parentClass).exists(_.isAbstractMember)
      yield
        val anonOverride = BinaryAnonOverride(binaryClass, overridden, binaryClass.symbol.declaredType)
        if method.isBridge then
          val tpe = overridden.declaredTypeAsSeenFrom(SkolemType(binaryClass.declaredType))
          BinaryMethodBridge(anonOverride, tpe)
        else anonOverride
    types.nextOption

  private def findAnonOverride(binaryClass: BinaryPartialFunction, method: binary.Method): BinaryMethodSymbol =
    val overriddenSym = defn.PartialFunctionClass.findNonOverloadedDecl(SimpleName(method.name))
    val tpe = overriddenSym.declaredTypeAsSeenFrom(SkolemType(binaryClass.declaredType))
    val anonOverride = BinaryAnonOverride(binaryClass, overriddenSym, tpe)
    if method.isBridge then BinaryMethodBridge(anonOverride, tpe)
    else anonOverride

  private def findBridgesAndMixinForwarders(binaryClass: BinaryClass, method: binary.Method): Seq[BinaryMethodSymbol] =
    if !binaryClass.symbol.isTrait then
      findBridgesAndMixinForwarders(binaryClass, binaryClass.symbol, binaryClass.symbol.thisType, method)
    else Seq.empty

  private def findBridgesAndMixinForwarders(
      binaryClass: BinaryClassSymbol,
      fromClass: ClassSymbol,
      fromType: Type,
      method: binary.Method
  ): Seq[BinaryMethodSymbol] =
    findBridges(binaryClass, fromClass, fromType, method).orIfEmpty(
      findMixinForwarder(binaryClass, fromClass, method) ++ findTraitParamAccessors(binaryClass, fromClass, method)
    )

  private def findBridges(
      binaryClass: BinaryClassSymbol,
      fromClass: ClassSymbol,
      fromType: Type,
      method: binary.Method
  ): Seq[BinaryMethodSymbol] =
    val overridingSyms = for
      classSym <- fromClass.linearization
      overriddenSym <- classSym.declarations.collect {
        case sym: TermSymbol if matchTargetName(method, sym) && matchSignature(method, sym) => sym
      }
      overridingSym = overriddenSym.overridingSymbolInLinearization(fromClass)
      if overridingSym.owner.isTrait || overridingSym.owner == fromClass
    yield (overridingSym, overriddenSym)
    overridingSyms.groupMapReduce(_(0))(_(1))((x, _) => x).toSeq.map { (overridingSym, overriddenSym) =>
      val target = BinaryMethod(binaryClass, overridingSym)
      val tpe = overriddenSym.declaredTypeAsSeenFrom(fromType)
      BinaryMethodBridge(target, tpe)
    }

  private def findMixinForwarder(
      binaryClass: BinaryClassSymbol,
      inClass: ClassSymbol,
      method: binary.Method
  ): Seq[BinaryMixinForwarder] =
    for
      traitSym <- inClass.linearization.filter(_.isTrait)
      sym <- traitSym.declarations
        .collect { case sym: TermSymbol if matchTargetName(method, sym) && matchSignature(method, sym) => sym }
      if sym.isOverridingSymbol(inClass)
    yield BinaryMixinForwarder(binaryClass, BinaryMethod(BinaryClass(traitSym), sym))

  private def findTraitParamAccessors(
      binaryClass: BinaryClassSymbol,
      fromClass: ClassSymbol,
      method: binary.Method
  ): Seq[BinaryTraitParamAccessor] =
    for
      traitSym <- fromClass.linearization.filter(_.isTrait)
      sym <- traitSym.declarations
        .collect { case sym: TermSymbol if matchTargetName(method, sym) && matchSignature(method, sym) => sym }
      if sym.isParamAccessor
    yield BinaryTraitParamAccessor(binaryClass, sym)

  private def notFound(symbol: binary.Symbol): Nothing = throw NotFoundException(symbol)

  private def ignore(symbol: binary.Symbol, reason: String): Nothing = throw IgnoredException(symbol, reason)

  private def unexpected(message: String): Nothing = throw UnexpectedException(message)

  private def withCompanionIfExtendsAnyVal(cls: ClassSymbol): Seq[ClassSymbol] =
    cls.companionClass match
      case Some(companionClass) if companionClass.isSubClass(defn.AnyValClass) =>
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
          .collect { case BinaryClass(sym) => sym }
        val sourceLines = removeInlinedLines(javaClass.sourceLines, classOwners)
        if javaClass.sourceLines.isEmpty || sourceLines.nonEmpty then
          classOwners
            .flatMap(cls => collectLocalClasses(cls, localClassName, sourceLines))
            .collect {
              case cls: BinaryClass if matchParents(cls.symbol, parents, javaClass.isInterface) => cls
              case samClass: BinarySAMClass if parents.contains(samClass.parentClass) => samClass
              case fun: BinaryPartialFunction
                  if parents == Set(defn.AbstractPartialFunctionClass, defn.SerializableClass) =>
                fun
            }
        else Seq.empty
      case Some(remaining) =>
        val localClasses = classOwners
          .flatMap(cls => collectLocalClasses(cls, localClassName, Seq.empty))
          .collect { case BinaryClass(cls) => cls }
        localClasses.flatMap(s => findClassRecursively(s, remaining))

  private def findClassRecursively(owner: DeclaringSymbol, decodedName: String): Seq[BinaryClass] =
    owner.declarations
      .collect { case sym: ClassSymbol => sym }
      .flatMap { sym =>
        val Symbol = s"${Regex.quote(sym.nameStr)}\\$$?(.*)".r
        decodedName match
          case Symbol(remaining) =>
            if remaining.isEmpty then Some(BinaryClass(sym))
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
            if lambda.samClassSymbol == defn.PartialFunctionClass then Some(BinaryPartialFunction(term, tpe))
            else Some(BinarySAMClass(term, lambda.samClassSymbol, tpe))
          case _ => None
    collectTrees(cls, lines) {
      case ClassDef(_, _, cls) if cls.isLocal && cls.nameStr == name => BinaryClass(cls)
      case SAMClassOrPartialFunction(binaryCls) => binaryCls
    }

  private def findAdaptedAnonFun(binaryClass: BinaryClassSymbol, method: binary.Method): Seq[BinaryMethodSymbol] =
    if method.instructions.nonEmpty then
      val underlying = method.instructions
        .collect {
          case Instruction.Method(_, owner, name, descriptor, _) if owner == method.declaringClass.name =>
            method.declaringClass.declaredMethod(name, descriptor)
        }
        .flatten
        .singleOrElse(unexpected(s"$method is not an adapted method: cannot find underlying invocation"))
      findAnonFunAndByNameArgs(binaryClass, underlying).map(BinaryAdaptedFun.apply)
    else findAnonFunAndByNameArgs(binaryClass, method, adapted = true)

  private def findAnonFunAndByNameArgs(
      binaryClass: BinaryClassSymbol,
      method: binary.Method,
      adapted: Boolean = false
  ): Seq[BinaryMethodSymbol] =
    val anonFuns = collectLocalMethods(binaryClass, method)(inlined => {
      case term if term.isAnonFun && matchSignature(method, term, checkTypeErasure = !adapted && !inlined) =>
        val anonFun = BinaryMethod(binaryClass, term)
        if adapted then BinaryAdaptedFun(anonFun) else anonFun
    })
    val byNameArgs =
      if method.allParameters.forall(_.isCapture) then findByNameArgs(binaryClass, method, adapted)
      else Seq.empty
    anonFuns ++ byNameArgs

  private object ByNameApply:
    def unapply(tree: Apply): Option[Seq[(TermTree, Type)]] =
      tree.fun.tpe.widenTermRef match
        case m: MethodType =>
          Some(tree.args.zip(m.paramTypes).collect { case (arg, t: ByNameType) => (arg, t.resultType) })
            .filter(_.nonEmpty)
        case _ => None

  private object InlineTree:
    def unapply(tree: Tree): Option[Symbol] =
      tree match
        case tree: TermReferenceTree if tree.symbol.isInline && tree.symbol.asTerm.isMethod =>
          Some(tree.symbol)
        case Apply(fun, _) => unapply(fun)
        case TypeApply(fun, _) => unapply(fun)
        case _ => None

  private def isInlineTree(tree: Tree): Boolean =
    InlineTree.unapply(tree).isDefined

  private def findByNameArgs(
      binaryClass: BinaryClassSymbol,
      method: binary.Method,
      adapted: Boolean
  ): Seq[BinaryMethodSymbol] =
    def matchType0(tpe: Type): Boolean =
      if adapted then tpe.erasedAsReturnType.toString == "void"
      else method.returnType.forall(matchType(tpe.erasedAsReturnType, _))
    val classOwners = getOwners(binaryClass)
    val sourceSpan = removeInlinedLines(method.sourceLines, classOwners).interval
    for
      classOwner <- classOwners
      byNameArg <- collectTrees1(classOwner, sourceSpan)(inlined => {
        case t @ ByNameApply(args) if !isInlineTree(t) =>
          args.collect {
            case (arg, paramTpe) if matchType0(paramTpe) && (inlined || matchLines(arg, sourceSpan)) =>
              val byNameArg = BinaryByNameArg(binaryClass, paramTpe)
              if adapted then BinaryAdaptedFun(byNameArg) else byNameArg
          }
      }).flatten
    yield byNameArg

  private def findByNameArgsProxy(binaryClass: BinaryClassSymbol, method: binary.Method): Seq[BinaryByNameArg] =
    def matchType0(tpe: TermType): Boolean =
      tpe match
        case tpe: Type => method.returnType.forall(matchType(tpe.erasedAsReturnType, _))
        case _ => false
    val classOwners = getOwners(binaryClass)
    val sourceSpan = removeInlinedLines(method.sourceLines, classOwners).interval
    val explicitByNameArgs =
      for
        classOwner <- classOwners
        byNameArg <- collectTrees1(classOwner, sourceSpan)(inlined => {
          case t @ ByNameApply(args) if isInlineTree(t) =>
            args.collect {
              case (arg, _) if inlined || (matchType0(arg.tpe) && matchLines(arg, sourceSpan)) =>
                BinaryByNameArg(binaryClass, arg.tpe.asInstanceOf)
            }
        }).flatten
      yield byNameArg
    val inlineOverrides = binaryClass.classSymbol.toSeq
      .flatMap(_.declarations)
      .collect {
        case term: TermSymbol
            if term.allOverriddenSymbols.nonEmpty &&
              term.isInline &&
              term.tree.exists(matchLines(_, method.sourceLines.interval)) =>
          term.declaredType.allParamTypes.collect {
            case byName: ByNameType if matchType0(byName.resultType) =>
              BinaryByNameArg(binaryClass, byName.resultType)
          }
      }
      .flatten
    explicitByNameArgs ++ inlineOverrides

  private def collectLocalMethods(
      binaryClass: BinaryClassSymbol,
      javaMethod: binary.Method
  )(
      matcher: Boolean => PartialFunction[TermSymbol, BinaryMethodSymbol]
  ): Seq[BinaryMethodSymbol] =
    val owners = getOwners(binaryClass)
    val sourceLines = removeInlinedLines(javaMethod.sourceLines, owners)
    for
      owner <- owners
      term <- collectTrees1(owner, sourceLines) { inlined =>
        val treeMatcher: PartialFunction[Tree, TermSymbol] = {
          case ValDef(_, _, _, sym) if sym.isLocal && sym.isModuleOrLazyVal => sym
          case DefDef(_, _, _, _, sym) if sym.isLocal => sym
        }
        treeMatcher.andThen(matcher(inlined))
      }
    yield term

  private def getOwners(binaryClass: BinaryClassSymbol): Seq[Symbol] =
    binaryClass match
      case BinaryClass(symbol) => withCompanionIfExtendsAnyVal(symbol)
      case BinarySyntheticCompanionClass(symbol) => Seq.empty
      case BinarySAMClass(symbol, _, _) => Seq(symbol)
      case BinaryPartialFunction(symbol, _) => Seq(symbol)

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
      paramTypes = superCons.declaredType.allParamTypes
      args = superCall.allArgsFlatten
      if args.size == paramTypes.size
      (arg, paramType) <- args.zip(paramTypes)
      if arg.pos.unknownOrContainsAll(span)
      argType0 <- asType(arg.tpe).toSeq
      argType = paramType match
        case byName: ByNameType => defn.Function0Type.appliedTo(byName.resultType)
        case _ => argType0
      if method.returnType.forall(matchType(argType.erasedAsReturnType, _))
    yield BinarySuperArg(binaryOwner, init, argType)
  end findSuperArgs

  private def findLiftedTry(binaryClass: BinaryClassSymbol, method: binary.Method): Seq[BinaryLiftedTry] =
    def matchType0(tpe: TermType): Boolean =
      tpe match
        case tpe: Type => method.returnType.forall(matchType(tpe.erasedAsReturnType, _))
        case _ => false
    val classOwners = getOwners(binaryClass)
    val sourceSpan = removeInlinedLines(method.sourceLines, classOwners).interval
    for
      classOwner <- classOwners
      liftedTry <- collectTrees[BinaryLiftedTry](classOwner, sourceSpan) {
        case tryTree: Try if matchType0(tryTree.tpe) =>
          BinaryLiftedTry(binaryClass, tryTree.tpe.asInstanceOf)
      }
    yield liftedTry

  private def collectTrees[S](cls: Symbol, lines: Seq[binary.SourceLine])(matcher: PartialFunction[Tree, S]): Seq[S] =
    collectTrees1(cls, lines)(_ => matcher)

  private def collectTrees1[S](root: Symbol, lines: Seq[binary.SourceLine])(
      matcher: Boolean => PartialFunction[Tree, S]
  ): Seq[S] =
    val span = lines.interval
    val collectors = Buffer.empty[Collector]
    var inlineSymbols = Set.empty[Symbol]

    class Collector(inlined: Boolean = false) extends TreeTraverser:
      collectors += this
      private var buffer = Map.empty[Tree, S]
      override def traverse(tree: Tree): Unit =
        if inlined || matchLines(tree, span) then
          matchTree(tree)
          tree match
            case InlineTree(symbol) if !inlineSymbols.contains(symbol) =>
              inlineSymbols += symbol
              val collector = new Collector(inlined = true)
              symbol.tree.foreach(collector.traverse)
            case _ => ()
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
      owners: Seq[Symbol]
  ): Seq[binary.SourceLine] =
    val inlineSymbols = owners.flatMap(collectInlineSymbols)
    sourceLines.filter(line =>
      owners.exists(_.pos.containsLine(line) && !inlineSymbols.exists(_.pos.containsLine(line)))
    )

  private def collectInlineSymbols(sym: Symbol): Seq[TermSymbol] =
    val buffer = Buffer.empty[TermSymbol]
    val collector = new TreeTraverser:
      override def traverse(tree: Tree): Unit =
        tree match
          case termDef: ValOrDefDef if termDef.symbol.isInline => buffer += termDef.symbol
          case _ => ()
        super.traverse(tree)
    sym.tree.foreach(collector.traverse)
    buffer.toSeq

  private def matchParents(classSymbol: ClassSymbol, expectedParents: Set[ClassSymbol], isInterface: Boolean): Boolean =
    if classSymbol.isEnum then expectedParents == classSymbol.parentClasses.toSet + defn.ProductClass
    else if isInterface then expectedParents == classSymbol.parentClasses.filter(_.isTrait).toSet
    else if classSymbol.isAnonClass then classSymbol.parentClasses.forall(expectedParents.contains)
    else expectedParents == classSymbol.parentClasses.toSet

  private def matchTargetName(method: binary.Method, symbol: TermSymbol): Boolean =
    method.unexpandedDecodedNames.contains(symbol.targetNameStr)

  private def matchSignature(
      method: binary.Method,
      symbol: TermSymbol,
      captureAllowed: Boolean = true,
      asJavaVarargs: Boolean = false,
      checkParamNames: Boolean = true,
      checkTypeErasure: Boolean = true
  ) =
    matchSignature1(
      method,
      symbol.declaredType,
      isAnonFun = symbol.isAnonFun,
      asJavaVarargs = asJavaVarargs,
      checkParamNames = checkParamNames,
      checkTypeErasure = checkTypeErasure
    )

  private def matchSignature1(
      method: binary.Method,
      declaredType: TypeOrMethodic,
      isAnonFun: Boolean,
      asJavaVarargs: Boolean = false,
      checkParamNames: Boolean = true,
      checkTypeErasure: Boolean = true
  ): Boolean =
    object CurriedContextFunction:
      def unapply(tpe: Type): Option[(Seq[TypeOrWildcard], TypeOrWildcard)] =
        def rec(tpe: TypeOrWildcard, args: Seq[TypeOrWildcard]): Option[(Seq[TypeOrWildcard], TypeOrWildcard)] =
          tpe match
            case tpe: AppliedType if tpe.tycon.isContextFunction => rec(tpe.args.last, args ++ tpe.args.init)
            case res => Option.when(args.nonEmpty)((args, res))
        rec(tpe, Seq.empty)

    val paramNames = declaredType.allParamNames.map(_.toString)
    val paramsSig = declaredType.allParamTypes.map(_.erasedAsArgType(asJavaVarargs))
    val resSig = declaredType.returnType.erasedAsReturnType

    if method.isConstructor && method.declaringClass.isJavaLangEnum then
      val declaredParams = method.allParameters.dropRight(2)
      val javaLangEnumParams = method.allParameters.takeRight(2)
      javaLangEnumParams.forall(_.isGenerated) &&
      declaredParams.size == paramNames.size &&
      (!checkParamNames || declaredParams.map(_.name).corresponds(paramNames)(_ == _)) &&
      (!checkTypeErasure || matchTypeErasure(paramsSig, resSig, declaredParams, method.returnType))
    else
      declaredType.returnType.dealias match
        case CurriedContextFunction(uncurriedArgs, uncurriedReturnType) if !isAnonFun =>
          val capturedParams = method.allParameters.dropRight(paramNames.size + uncurriedArgs.size)
          val declaredParams = method.allParameters.drop(capturedParams.size).dropRight(uncurriedArgs.size)
          val contextParams = method.allParameters.drop(capturedParams.size + declaredParams.size)

          (capturedParams ++ contextParams).forall(_.isGenerated) &&
          declaredParams.size == paramNames.size &&
          (!checkParamNames || declaredParams.map(_.name).corresponds(paramNames)(_ == _)) &&
          (!checkTypeErasure || matchTypeErasure(
            paramsSig ++ uncurriedArgs.map(_.erasedAsArgType(asJavaVarargs)),
            uncurriedReturnType.erasedAsReturnType,
            declaredParams ++ contextParams,
            method.returnType
          ))
        case _ =>
          val capturedParams = method.allParameters.dropRight(paramNames.size)
          val declaredParams = method.allParameters.drop(capturedParams.size)

          capturedParams.forall(_.isGenerated) &&
          declaredParams.size == paramNames.size &&
          (!checkParamNames || declaredParams.map(_.name).corresponds(paramNames)(_ == _)) &&
          (!checkTypeErasure || matchTypeErasure(paramsSig, resSig, declaredParams, method.returnType))

  private def matchTypeErasure(
      scalaParams: Seq[SignatureName],
      scalaReturnType: SignatureName,
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
      scalaType: SignatureName,
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
    rec(signatureNameToString(scalaType), javaType.name)

  private def signatureNameToString(sigName: SignatureName): String =
    sigName.items
      .map {
        case ObjectClassName(underlying) => underlying
        case name => name
      }
      .mkString(".")
