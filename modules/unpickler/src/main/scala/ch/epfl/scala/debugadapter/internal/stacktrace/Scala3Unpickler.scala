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
import tastyquery.SourcePosition

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
      case _: BinaryGetter => true
      case _: BinarySuperAccessor => true
      case _: BinarySpecializedMethod => true
      case _: BinaryInlineAccessor => true
      case _: BinaryAdaptedFun => true
      case _: BinarySAMClassConstructor => true
      case _ => false

  def formatMethod(obj: Any): Optional[String] =
    formatMethod(JdiMethod(obj)).toJava

  def formatMethod(method: binary.Method): Option[String] =
    findMethod(method) match
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
      find { case Patterns.SpecializedMethod(names) =>
        requiresBinaryClass(findSpecializedMethod(_, method, names))
      }
        .orFind { case _ if method.isBridge => findBridgesAndMixinForwarders(binaryClass, method).toSeq }
        .orFind { case Patterns.LocalLazyInit(names) =>
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
        .orFind { case Patterns.LocalMethod(names) => findLocalMethods(binaryClass, method, names) }
        .orFind { case Patterns.LazyInit(name) => requiresBinaryClass(findLazyInit(_, name)) }
        .orFind { case Patterns.Outer(_) => Seq(findOuter(binaryClass)) }
        .orFind { case Patterns.TraitInitializer() => requiresBinaryClass(findTraitInitializer(_, method)) }
        .orFind { case Patterns.InlineAccessor(names) =>
          names.flatMap(findInlineAccessorOrForwarder(binaryClass, method, _))
        }
        .orFind { case Patterns.ValueClassExtension() => requiresBinaryClass(findValueClassExtension(_, method)) }
        .orFind { case Patterns.DeserializeLambda() =>
          Seq(BinaryDeserializeLambda(binaryClass, defn.DeserializeLambdaType))
        }
        .orFind { case Patterns.ParamForwarder(names) => requiresBinaryClass(findParamForwarder(_, method, names)) }
        .orFind { case Patterns.TraitSetter(name) => requiresBinaryClass(findTraitSetter(_, method, name)) }
        .orFind { case Patterns.Setter(names) =>
          findStandardMethods(binaryClass, method).orIfEmpty(requiresBinaryClass(findSetter(_, method, names)))
        }
        .orFind { case Patterns.SuperAccessor(names) => requiresBinaryClass(findSuperAccessor(_, method, names)) }
        .orFind { case Patterns.TraitStaticForwarder(names) =>
          requiresBinaryClass(findTraitStaticForwarder(_, method).toSeq)
        }
        .orFind {
          case _ if method.isStatic && binaryClass.isJava => findStaticJavaMethods(binaryClass, method)
          case _ if method.isStatic => findStaticForwarder(binaryClass, method)
          case _ => findStandardMethods(binaryClass, method)
        }

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
      else if cls.sourceLines.forall(_.isEmpty) && allSymbols.forall(_.symbol.isModuleClass) then
        allSymbols.collect { case BinaryClass(symbol) => BinarySyntheticCompanionClass(symbol) }
      else allSymbols.filter(!_.symbol.isModuleClass)
    candidates.singleOrThrow(cls)

  private def findStaticJavaMethods(binaryClass: BinaryClassSymbol, method: binary.Method): Seq[BinaryMethodSymbol] =
    binaryClass.companionClass.toSeq
      .flatMap(_.symbol.declarations)
      .collect {
        case sym: TermSymbol if matchTargetName(method, sym) && matchSignature(method, sym, checkParamNames = false) =>
          BinaryMethod(binaryClass, sym)
      }

  private def findStandardMethods(binaryClass: BinaryClassSymbol, method: binary.Method): Seq[BinaryMethodSymbol] =
    binaryClass match
      case samClass: BinarySAMClass =>
        if method.isConstructor then Seq(BinarySAMClassConstructor(samClass, samClass.declaredType))
        else findAnonOverride(samClass, method).toSeq
      case partialFun: BinaryPartialFunction =>
        if method.isConstructor then Seq(BinarySAMClassConstructor(partialFun, partialFun.declaredType))
        else Seq(findAnonOverride(partialFun, method))
      case binaryClass: BinaryClass => findInstanceMethods(binaryClass, method)
      case syntheticClass: BinarySyntheticCompanionClass => Seq.empty

  private def findParamForwarder(
      binaryClass: BinaryClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[BinaryMethod] =
    binaryClass.symbol.declarations.collect {
      case sym: TermSymbol if names.contains(sym.targetNameStr) && matchSignature(method, sym) =>
        BinaryMethod(binaryClass, sym)
    }

  private def findTraitSetter(binaryClass: BinaryClass, method: binary.Method, name: String): Seq[BinarySetter] =
    for
      traitSym <- binaryClass.symbol.linearization.filter(_.isTrait)
      if method.decodedName.contains("$" + traitSym.nameStr + "$")
      sym <- traitSym.declarations.collect {
        case sym: TermSymbol if sym.targetNameStr == name && !sym.isMethod && !sym.isAbstractMember => sym
      }
      paramType <- sym.declaredTypeAsSeenFrom(binaryClass.symbol.thisType) match
        case tpe: Type => Some(tpe)
        case _ => None
    yield
      val tpe = MethodType(List(SimpleName("x$1")), List(paramType), defn.UnitType)
      BinarySetter(binaryClass, sym, tpe)

  private def findSetter(binaryClass: BinaryClass, method: binary.Method, names: Seq[String]): Seq[BinarySetter] =
    val javaParamType = method.allParameters.last.`type`
    def matchType0(sym: TermSymbol): Boolean =
      matchSetterArgType(sym.declaredType, javaParamType)
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
      if method.decodedName.contains("$" + traitSym.nameStr + "$")
      sym <- traitSym.declarations.collect {
        case sym: TermSymbol if names.contains(sym.targetNameStr) && !sym.isAbstractMember => sym
      }
      expectedTpe = sym.declaredTypeAsSeenFrom(binaryClass.symbol.thisType)
      if matchSignature1(method, expectedTpe)
    yield
      val tpe = sym.declaredTypeAsSeenFrom(binaryClass.symbol.thisType)
      BinarySuperAccessor(binaryClass, sym, tpe)

  private def findSpecializedMethod(
      binaryClass: BinaryClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[BinarySpecializedMethod] =
    binaryClass.symbol.declarations.collect {
      case sym: TermSymbol
          if names.contains(sym.targetNameStr) &&
            matchSignature(method, sym, captureAllowed = false, checkParamNames = false, checkTypeErasure = false) &&
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
        if matchSignature1(method, resultType, checkParamNames = checkParamNames)
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
            if matchSetterArgType(sym.declaredType, javaParamType)
          yield
            val declaredTpe =
              MethodType(List(SimpleName("x$1")), List(sym.declaredType.asInstanceOf[Type]), defn.UnitType)
            BinarySetter(BinaryClass(classSym), sym, declaredTpe)
        case _ => Seq.empty
    standardMethods.orIfEmpty(setters)

  private def findInstanceMethods(binaryClass: BinaryClass, method: binary.Method): Seq[BinaryMethodSymbol] =
    if method.isConstructor && binaryClass.symbol.isSubClass(defn.AnyValClass) then
      binaryClass.symbol.getAllOverloadedDecls(SimpleName("<init>")).map(BinaryMethod(binaryClass, _))
    else
      val isJava = binaryClass.isJava
      val fromClass = binaryClass.symbol.declarations
        .collect { case sym: TermSymbol if matchTargetName(method, sym) => sym }
        .collect {
          case sym
              if matchSignature(
                method,
                sym,
                asJavaVarargs = isJava,
                captureAllowed = !isJava,
                checkParamNames = !isJava
              ) =>
            BinaryMethod(binaryClass, sym)
          case sym if !isJava && matchSignature(method, sym, asJavaVarargs = true) =>
            BinaryMethodBridge(BinaryMethod(binaryClass, sym), sym.declaredType)
        }
      fromClass.orIfEmpty(findAccessorsFromTraits(binaryClass, method))

  private def findAccessorsFromTraits(binaryClass: BinaryClass, method: binary.Method): Seq[BinaryMethodSymbol] =
    if binaryClass.symbol.isTrait then Seq.empty
    else findAccessorsFromTraits(binaryClass, binaryClass.symbol, binaryClass.symbol.thisType, method)

  private def findAccessorsFromTraits(
      binaryClass: BinaryClassSymbol,
      fromClass: ClassSymbol,
      fromType: Type,
      method: binary.Method
  ): Seq[BinaryMethodSymbol] =
    for
      traitSym <- fromClass.linearization.filter(_.isTrait)
      sym <- traitSym.declarations
        .collect { case sym: TermSymbol if matchTargetName(method, sym) && matchSignature(method, sym) => sym }
      if sym.isParamAccessor || sym.isSetter || !sym.isMethod
      if sym.isOverridingSymbol(fromClass)
    yield
      val tpe = sym.declaredTypeAsSeenFrom(fromType)
      if sym.isParamAccessor then BinaryTraitParamAccessor(binaryClass, sym)
      else if sym.isSetter then BinarySetter(binaryClass, sym, tpe)
      else BinaryGetter(binaryClass, sym, tpe)

  private def findLocalMethods(
      binaryClass: BinaryClassSymbol,
      method: binary.Method,
      names: Seq[String]
  ): Seq[BinaryMethodSymbol] =
    collectLocalMethods(binaryClass, method)(inlined => {
      case term if names.contains(term.nameStr) && matchSignature(method, term, checkTypeErasure = !inlined) =>
        BinaryMethod(binaryClass, term)
    })

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
      method: binary.Method
  ): Option[BinaryTraitStaticForwarder] =
    method.instructions
      .collect {
        case Instruction.Method(_, owner, name, descriptor, _) if owner == method.declaringClass.name =>
          method.declaringClass.method(name, descriptor)
      }
      .singleOpt
      .flatten
      .map(target => BinaryTraitStaticForwarder(findMethod(binaryClass, target)))

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

  private def findStaticForwarder(binaryClass: BinaryClassSymbol, method: binary.Method): Seq[BinaryStaticForwarder] =
    binaryClass.companionClass.toSeq.flatMap(findStaticForwarder(binaryClass, _, method))

  private def findStaticForwarder(
      binaryClass: BinaryClassSymbol,
      companionObject: BinaryClass,
      method: binary.Method
  ): Seq[BinaryStaticForwarder] =
    method.instructions
      .collect { case Instruction.Method(_, owner, name, descriptor, _) =>
        binaryClassLoader.loadClass(owner).method(name, descriptor)
      }
      .flatten
      .singleOpt
      .toSeq
      .map(findMethod)
      .collect {
        case BinaryMixinForwarder(binaryOwner, target) => target
        case target => target
      }
      .map { target =>
        val declaredType = target.termSymbol
          .map(_.declaredTypeAsSeenFrom(companionObject.symbol.thisType))
          .getOrElse(target.declaredType)
        BinaryStaticForwarder(binaryClass, target, declaredType)
      }

  private def findAnonOverride(binaryClass: BinarySAMClass, method: binary.Method): Option[BinaryMethodSymbol] =
    val types =
      for
        parentCls <- binaryClass.parentClass.linearization.iterator
        overridden <- parentCls.declarations.collect { case term: TermSymbol if matchTargetName(method, term) => term }
        if overridden.overridingSymbol(binaryClass.parentClass).exists(_.isAbstractMember)
      yield BinaryAnonOverride(binaryClass, overridden, binaryClass.symbol.declaredType)
    types.nextOption

  private def findAnonOverride(binaryClass: BinaryPartialFunction, method: binary.Method): BinaryMethodSymbol =
    val overriddenSym = defn.PartialFunctionClass.findNonOverloadedDecl(SimpleName(method.name))
    val tpe = overriddenSym.declaredTypeAsSeenFrom(SkolemType(binaryClass.declaredType))
    BinaryAnonOverride(binaryClass, overriddenSym, tpe)

  private def findBridgesAndMixinForwarders(
      binaryClass: BinaryClassSymbol,
      method: binary.Method
  ): Option[BinaryMethodSymbol] =
    binaryClass match
      case BinaryClass(symbol) if !symbol.isTrait =>
        findBridgesAndMixinForwarders(binaryClass, symbol, symbol.thisType, method)
      case BinarySAMClass(_, parentClass, declaredType) =>
        findBridgesAndMixinForwarders(binaryClass, parentClass, SkolemType(declaredType), method)
      case BinaryPartialFunction(_, declaredType) =>
        findBridgesAndMixinForwarders(binaryClass, defn.PartialFunctionClass, SkolemType(declaredType), method)
      case _ => None

  private def findBridgesAndMixinForwarders(
      binaryClass: BinaryClassSymbol,
      fromClass: ClassSymbol,
      fromType: Type,
      method: binary.Method
  ): Option[BinaryMethodSymbol] =
    findBridges(binaryClass, fromClass, fromType, method)
      .orIfEmpty(findMixinForwarder(binaryClass, method))

  private def findBridges(
      binaryClass: BinaryClassSymbol,
      fromClass: ClassSymbol,
      fromType: Type,
      method: binary.Method
  ): Option[BinaryMethodSymbol] =
    method.instructions
      .collect {
        case Instruction.Method(_, owner, name, descriptor, _) if name == method.name =>
          binaryClassLoader.loadClass(owner).method(name, descriptor)
      }
      .singleOpt
      .flatten
      .map { binaryTarget =>
        val target = findMethod(binaryTarget)
        val tpe = target.declaredType.asSeenFrom(fromType, fromClass)
        BinaryMethodBridge(target, tpe)
      }

  private def findMixinForwarder(
      binaryClass: BinaryClassSymbol,
      method: binary.Method
  ): Option[BinaryMixinForwarder] =
    method.instructions
      .collect { case Instruction.Method(_, owner, name, descriptor, _) =>
        binaryClassLoader.loadClass(owner).method(name, descriptor)
      }
      .singleOpt
      .flatten
      .filter(target => target.isStatic && target.declaringClass.isInterface)
      .map(findMethod)
      .collect { case BinaryTraitStaticForwarder(target) => BinaryMixinForwarder(binaryClass, target) }

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
        val sourceLines = javaClass.sourceLines.map(removeInlinedLines(_, classOwners))
        classOwners
          .flatMap(cls => collectLocalClasses(cls, localClassName, sourceLines))
          .collect {
            case cls: BinaryClass if matchParents(cls.symbol, parents, javaClass.isInterface) => cls
            case samClass: BinarySAMClass if parents.contains(samClass.parentClass) => samClass
            case fun: BinaryPartialFunction
                if parents == Set(defn.AbstractPartialFunctionClass, defn.SerializableClass) =>
              fun
          }
      case Some(remaining) =>
        val localClasses = classOwners
          .flatMap(cls => collectLocalClasses(cls, localClassName, None))
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
      lines: Option[binary.SourceLines]
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
      Some(tree.args.zip(tree.methodType.paramTypes).collect { case (arg, t: ByNameType) => (arg, t.resultType) })
        .filter(_.nonEmpty)

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
      else matchReturnType(tpe, method.returnType)
    val classOwners = getOwners(binaryClass)
    val sourceLines =
      if classOwners.size == 2 && method.allParameters.filter(p => p.name.matches("\\$this\\$\\d+")).nonEmpty then
        // workaround of https://github.com/lampepfl/dotty/issues/18816
        method.sourceLines.map(removeInlinedLines(_, classOwners).last)
      else method.sourceLines.map(removeInlinedLines(_, classOwners))
    for
      classOwner <- classOwners
      byNameArg <- collectTrees1(classOwner, sourceLines)(inlined => {
        case t @ ByNameApply(args) if !isInlineTree(t) =>
          args.collect {
            case (arg, paramTpe) if matchType0(paramTpe) && (inlined || sourceLines.forall(matchLines(arg, _))) =>
              val byNameArg = BinaryByNameArg(binaryClass, paramTpe)
              if adapted then BinaryAdaptedFun(byNameArg) else byNameArg
          }
      }).flatten
    yield byNameArg

  private def findByNameArgsProxy(binaryClass: BinaryClassSymbol, method: binary.Method): Seq[BinaryByNameArg] =
    def matchType0(tpe: TermType): Boolean =
      matchReturnType(tpe, method.returnType)
    val classOwners = getOwners(binaryClass)
    val sourceLines = method.sourceLines.map(removeInlinedLines(_, classOwners))
    val explicitByNameArgs =
      for
        classOwner <- classOwners
        byNameArg <- collectTrees1(classOwner, sourceLines)(inlined => {
          case t @ ByNameApply(args) if isInlineTree(t) =>
            args.collect {
              case (arg, _) if inlined || (matchType0(arg.tpe) && sourceLines.forall(matchLines(arg, _))) =>
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
              method.sourceLines.forall(sourceLines => term.tree.exists(matchLines(_, sourceLines))) =>
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
    val sourceLines =
      if owners.size == 2 && javaMethod.allParameters.filter(p => p.name.matches("\\$this\\$\\d+")).nonEmpty then
        // workaround of https://github.com/lampepfl/dotty/issues/18816
        javaMethod.sourceLines.map(removeInlinedLines(_, owners).last)
      else javaMethod.sourceLines.map(removeInlinedLines(_, owners))
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

    val sourceLines = method.sourceLines
    val localClasses = collectTrees(binaryOwner.symbol, sourceLines) { case ClassDef(_, _, cls) if cls.isLocal => cls }
    val innerClasses = binaryOwner.symbol.declarations.collect {
      case cls: ClassSymbol if sourceLines.forall(matchLines(cls.pos, _)) => cls
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
      if sourceLines.forall(matchLines(arg, _))
      argType0 <- asType(arg.tpe).toSeq
      argType = paramType match
        case byName: ByNameType => defn.Function0Type.appliedTo(byName.resultType)
        case _ => argType0
      if matchReturnType(argType, method.returnType)
    yield BinarySuperArg(binaryOwner, init, argType)
  end findSuperArgs

  private def findLiftedTry(binaryClass: BinaryClassSymbol, method: binary.Method): Seq[BinaryLiftedTry] =
    def matchType0(tpe: TermType): Boolean =
      matchReturnType(tpe, method.returnType)
    val classOwners = getOwners(binaryClass)
    val sourceLines = method.sourceLines.map(removeInlinedLines(_, classOwners))
    for
      classOwner <- classOwners
      liftedTry <- collectTrees[BinaryLiftedTry](classOwner, sourceLines) {
        case tryTree: Try if matchType0(tryTree.tpe) =>
          BinaryLiftedTry(binaryClass, tryTree.tpe.asInstanceOf)
      }
    yield liftedTry

  private def collectTrees[S](cls: Symbol, lines: Option[binary.SourceLines])(
      matcher: PartialFunction[Tree, S]
  ): Seq[S] =
    collectTrees1(cls, lines)(_ => matcher)

  private def collectTrees1[S](root: Symbol, sourceLines: Option[binary.SourceLines])(
      matcher: Boolean => PartialFunction[Tree, S]
  ): Seq[S] =
    val collectors = Buffer.empty[Collector]
    var inlineSymbols = Set.empty[Symbol]

    class Collector(inlined: Boolean = false) extends TreeTraverser:
      collectors += this
      private var buffer = Map.empty[Tree, S]
      override def traverse(tree: Tree): Unit =
        if inlined || sourceLines.forall(matchLines(tree, _)) then
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
        if inlined || sourceLines.forall(_.isEmpty) then buffer.values.toSeq
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

  private def matchLines(tree: Tree, sourceLines: binary.SourceLines): Boolean =
    tree match
      case lambda: Lambda => lambda.meth.symbol.tree.exists(matchLines(_, sourceLines))
      case tree => matchLines(tree.pos, sourceLines)

  private def matchLines(pos: SourcePosition, sourceLines: binary.SourceLines): Boolean =
    pos.isUnknown
      || !pos.hasLineColumnInformation
      || pos.sourceFile.name != sourceLines.sourceName
      || sourceLines.tastySpan.forall(line => pos.startLine <= line && pos.endLine >= line)

  private def removeInlinedLines(
      sourceLines: binary.SourceLines,
      owners: Seq[Symbol]
  ): binary.SourceLines =
    val inlineSymbols = owners.flatMap(collectInlineSymbols)
    sourceLines.filterTasty(line =>
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
    method.unexpandedDecodedNames.map(_.stripSuffix("$")).contains(symbol.targetNameStr)

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
      uncurryContextFunction = !symbol.isAnonFun,
      captureAllowed = captureAllowed,
      asJavaVarargs = asJavaVarargs,
      checkParamNames = checkParamNames,
      checkTypeErasure = checkTypeErasure
    )

  private def matchSignature1(
      method: binary.Method,
      declaredType: TypeOrMethodic,
      uncurryContextFunction: Boolean = true,
      captureAllowed: Boolean = true,
      asJavaVarargs: Boolean = false,
      checkParamNames: Boolean = true,
      checkTypeErasure: Boolean = true
  ): Boolean =
    /* After code generation, a method ends up with more than its declared parameters.
     *
     * It has, in order:
     *
     * - capture params,
     * - declared params,
     * - "expanded" params (from java.lang.Enum constructors and uncurried context function types).
     *
     * We can only check the names of declared params.
     * We can check the (erased) type of declared and expand params; together we call them "regular" params.
     */

    def expandContextFunctions(tpe: Type, acc: List[Type]): (List[Type], Type) =
      tpe.dealias match
        case tpe: AppliedType if tpe.tycon.isContextFunction =>
          val argsAsTypes = tpe.args.map(_.highIfWildcard)
          expandContextFunctions(argsAsTypes.last, acc ::: argsAsTypes.init)
        case _ =>
          (acc, tpe)

    val declaredParamTypes = declaredType.allParamTypes

    // Compute the expected expanded params and return type
    val (expandedParamTypes, returnType) =
      if method.isConstructor && method.declaringClass.isJavaLangEnum then
        (List(defn.StringType, defn.IntType), declaredType.returnType)
      else if uncurryContextFunction then expandContextFunctions(declaredType.returnType, acc = Nil)
      else (Nil, declaredType.returnType)

    val regularParamTypes = declaredParamTypes ::: expandedParamTypes

    // Do the matching
    val capturedParamCount = method.allParameters.size - regularParamTypes.size
    if capturedParamCount < 0 then
      // not enough parameters
      false
    else if !captureAllowed && capturedParamCount > 0 then
      // there are captures but they are not allowed
      false
    else
      // split the method parameters into captured, declared and expanded
      val (capturedParams, regularParams) = method.allParameters.splitAt(capturedParamCount)
      val (declaredParams, expandedParams) = regularParams.splitAt(declaredParamTypes.size)

      def matchNames(): Boolean =
        declaredType.allParamNames.corresponds(declaredParams)((name, javaParam) => name.toString() == javaParam.name)

      def matchErasedTypes(): Boolean =
        regularParamTypes.corresponds(regularParams)((tpe, javaParam) =>
          matchArgType(tpe, javaParam.`type`, asJavaVarargs)
        ) &&
          matchReturnType(returnType, method.returnType)

      capturedParams.forall(_.isGenerated) && // captures are generated
      expandedParams.forall(_.isGenerated) && // expanded params are generated
      (!checkParamNames || matchNames()) &&
      (!checkTypeErasure || matchErasedTypes())

  private lazy val scalaPrimitivesToJava: Map[ClassSymbol, String] = Map(
    ctx.defn.BooleanClass -> "boolean",
    ctx.defn.ByteClass -> "byte",
    ctx.defn.CharClass -> "char",
    ctx.defn.DoubleClass -> "double",
    ctx.defn.FloatClass -> "float",
    ctx.defn.IntClass -> "int",
    ctx.defn.LongClass -> "long",
    ctx.defn.ShortClass -> "short",
    ctx.defn.UnitClass -> "void",
    ctx.defn.NullClass -> "scala.runtime.Null$"
  )

  private def matchSetterArgType(scalaVarType: TypeOrMethodic, javaSetterParamType: binary.Type): Boolean =
    scalaVarType match
      case scalaVarType: Type => matchType(scalaVarType.erasedAsArgType(asJavaVarargs = false), javaSetterParamType)
      case _: MethodicType => false

  private def matchArgType(scalaType: Type, javaType: binary.Type, asJavaVarargs: Boolean): Boolean =
    matchType(scalaType.erasedAsArgType(asJavaVarargs), javaType)

  private def matchReturnType(scalaType: TermType, javaType: Option[binary.Type]): Boolean =
    scalaType match
      case scalaType: Type => javaType.forall(matchType(scalaType.erasedAsReturnType, _))
      case _: MethodicType | _: PackageRef => false

  private lazy val dollarDigitsMaybeDollarAtEndRegex = "\\$\\d+\\$?$".r

  private def matchType(
      scalaType: ErasedTypeRef,
      javaType: binary.Type
  ): Boolean =
    def rec(scalaType: ErasedTypeRef, javaType: String): Boolean =
      scalaType match
        case ErasedTypeRef.ArrayTypeRef(base, dimensions) =>
          javaType.endsWith("[]" * dimensions) &&
          rec(base, javaType.dropRight(2 * dimensions))

        case ErasedTypeRef.ClassRef(scalaClass) =>
          scalaPrimitivesToJava.get(scalaClass) match
            case Some(javaPrimitive) => javaPrimitive == javaType
            case None => matchClassType(scalaClass, javaType, nested = false)

    rec(scalaType, javaType.name)

  private def matchClassType(scalaClass: ClassSymbol, javaType: String, nested: Boolean): Boolean =
    def encodedName(nested: Boolean): String = scalaClass.name match
      case ObjectClassTypeName(underlying) if nested => NameTransformer.encode(underlying.toString())
      case name => NameTransformer.encode(name.toString())

    scalaClass.owner match
      case owner: PackageSymbol =>
        javaType == owner.fullName.toString() + "." + encodedName(nested)

      case owner: ClassSymbol =>
        val encodedName1 = encodedName(nested)
        javaType.endsWith("$" + encodedName1) &&
        matchClassType(owner, javaType.dropRight(1 + encodedName1.length()), nested = true)

      case owner: TermOrTypeSymbol =>
        dollarDigitsMaybeDollarAtEndRegex.findFirstIn(javaType).exists { suffix =>
          val prefix = javaType.stripSuffix(suffix)
          val encodedName1 = encodedName(nested = true)
          prefix.endsWith("$" + encodedName1) && {
            val ownerJavaType = prefix.dropRight(1 + encodedName1.length())
            enclosingClassOwners(owner).exists(matchClassType(_, ownerJavaType, nested = true))
          }
        }

  private def enclosingClassOwners(sym: TermOrTypeSymbol): List[ClassSymbol] =
    sym.owner match
      case owner: ClassSymbol => owner :: enclosingClassOwners(owner)
      case owner: TermOrTypeSymbol => enclosingClassOwners(owner)
      case owner: PackageSymbol => Nil
