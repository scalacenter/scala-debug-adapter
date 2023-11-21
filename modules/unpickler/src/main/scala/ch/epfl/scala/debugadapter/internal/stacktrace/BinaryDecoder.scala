package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.internal.binary
import ch.epfl.scala.debugadapter.internal.javareflect.JavaReflectLoader
import ch.epfl.scala.debugadapter.internal.stacktrace.*
import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Signatures.*
import tastyquery.SourcePosition
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.jdk.ClasspathLoaders

import java.nio.file.Path
import scala.util.matching.Regex

object BinaryDecoder:
  def apply(classEntries: Seq[Path], javaRuntime: Seq[Path]): BinaryDecoder =
    val classLoader = JavaReflectLoader(classEntries)
    val classpath = ClasspathLoaders.read(classEntries.toList ++ javaRuntime)
    val ctx = Context.initialize(classpath)
    new BinaryDecoder(classLoader)(using ctx)

final class BinaryDecoder(private[stacktrace] val classLoader: binary.BinaryClassLoader)(using Context):
  private given defn: Definitions = Definitions()

  def decodeClass(cls: binary.ClassType): DecodedClass =
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
        reduceAmbiguityOnClasses(findLocalClasses(cls, packageSym, decl, "$anon", remaining))
      case Patterns.LocalClass(declaringClassName, localClassName, remaining) =>
        findLocalClasses(cls, packageSym, declaringClassName, localClassName, remaining)
      case _ => findClassRecursively(packageSym, decodedClassName)

    val candidates =
      if cls.isObject then allSymbols.filter(_.isModuleClass)
      else if cls.sourceLines.forall(_.isEmpty) && allSymbols.forall(_.isModuleClass) then
        allSymbols.collect { case cls: DecodedClass.ClassDef => DecodedClass.SyntheticCompanionClass(cls.symbol) }
      else allSymbols.filter(!_.isModuleClass)
    candidates.singleOrThrow(cls)
  end decodeClass

  def decodeMethod(method: binary.Method): DecodedMethod =
    val decodedClass = decodeClass(method.declaringClass)
    decodeMethod(decodedClass, method)

  def decodeMethod(decodedClass: DecodedClass, method: binary.Method): DecodedMethod =
    def find(f: PartialFunction[binary.Method, Seq[DecodedMethod]]): Seq[DecodedMethod] =
      f.applyOrElse(method, _ => Seq.empty[DecodedMethod])

    extension (xs: Seq[DecodedMethod])
      def orFind(f: PartialFunction[binary.Method, Seq[DecodedMethod]]): Seq[DecodedMethod] =
        if xs.nonEmpty then xs else f.applyOrElse(method, _ => Seq.empty[DecodedMethod])
    val candidates =
      find { case Patterns.SpecializedMethod(names) => findSpecializedMethod(decodedClass, method, names) }
        .orFind { case _ if method.isBridge => findBridgesAndMixinForwarders(decodedClass, method).toSeq }
        .orFind { case Patterns.LocalLazyInit(names) =>
          collectLocalMethods(decodedClass, method) {
            case term if term.symbol.isModuleOrLazyVal && names.contains(term.symbol.nameStr) =>
              wrapIfInline(term, DecodedMethod.LazyInit(decodedClass, term.symbol))
          }
        }
        .orFind { case Patterns.AnonFun(_) => findAnonFunsAndReduceAmbiguity(decodedClass, method) }
        .orFind { case Patterns.AdaptedAnonFun(_) => findAdaptedAnonFun(decodedClass, method) }
        .orFind { case Patterns.ByNameArgProxy() => findByNameArgsProxy(decodedClass, method) }
        .orFind { case Patterns.SuperArg() => findSuperArgs(decodedClass, method) }
        .orFind { case Patterns.LiftedTree() => findLiftedTries(decodedClass, method) }
        .orFind { case Patterns.LocalMethod(names) => findLocalMethods(decodedClass, method, names) }
        .orFind { case Patterns.LazyInit(name) => findLazyInit(decodedClass, name) }
        .orFind { case Patterns.Outer(_) => findOuter(decodedClass).toSeq }
        .orFind { case Patterns.TraitInitializer() => findTraitInitializer(decodedClass, method) }
        .orFind { case Patterns.InlineAccessor(names) => findInlineAccessor(decodedClass, method, names).toSeq }
        .orFind { case Patterns.ValueClassExtension() => findValueClassExtension(decodedClass, method) }
        .orFind { case Patterns.DeserializeLambda() =>
          Seq(DecodedMethod.DeserializeLambda(decodedClass, defn.DeserializeLambdaType))
        }
        .orFind { case Patterns.ParamForwarder(names) => findParamForwarder(decodedClass, method, names) }
        .orFind { case Patterns.TraitSetter(name) => findTraitSetter(decodedClass, method, name) }
        .orFind { case Patterns.Setter(names) =>
          findStandardMethods(decodedClass, method).orIfEmpty(findSetter(decodedClass, method, names))
        }
        .orFind { case Patterns.SuperAccessor(names) => findSuperAccessor(decodedClass, method, names) }
        .orFind { case Patterns.TraitStaticForwarder(names) => findTraitStaticForwarder(decodedClass, method).toSeq }
        .orFind {
          case _ if method.isStatic && decodedClass.isJava => findStaticJavaMethods(decodedClass, method)
          case _ if method.isStatic => findStaticForwarder(decodedClass, method)
          case _ => findStandardMethods(decodedClass, method)
        }

    candidates.singleOrThrow(method)
  end decodeMethod

  private def reduceAmbiguityOnClasses(syms: Seq[DecodedClass]): Seq[DecodedClass] =
    if syms.size > 1 then
      val reduced = syms.filterNot(sym => syms.exists(enclose(sym, _)))
      if reduced.size != 0 then reduced else syms
    else syms

  private def enclose(enclosing: DecodedClass, enclosed: DecodedClass): Boolean =
    (enclosing, enclosed) match
      case (enclosing: DecodedClass.InlinedClass, enclosed: DecodedClass.InlinedClass) =>
        enclosing.callPos.enclose(enclosed.callPos) || (
          !enclosed.callPos.enclose(enclosing.callPos) &&
            enclose(enclosing.underlying, enclosed.underlying)
        )
      case (enclosing: DecodedClass.InlinedClass, enclosed) =>
        enclosing.callPos.enclose(enclosed.pos)
      case (enclosing, enclosed: DecodedClass.InlinedClass) =>
        enclosing.pos.enclose(enclosed.callPos)
      case (enclosing, enclosed) =>
        enclosing.pos.enclose(enclosed.pos)

  private def findLocalClasses(
      javaClass: binary.ClassType,
      packageSym: PackageSymbol,
      declaringClassName: String,
      localClassName: String,
      remaining: Option[String]
  ): Seq[DecodedClass] =
    val classOwners = findClassRecursively(packageSym, declaringClassName).map(_.symbol)
    remaining match
      case None =>
        val parents = (javaClass.superclass.toSet ++ javaClass.interfaces)
          .map(decodeClass(_))
          .collect { case cls: DecodedClass.ClassDef => cls.symbol }
        classOwners
          .flatMap(cls => collectLocalClasses(cls, localClassName, javaClass.sourceLines))
          .filter(matchParents(_, parents, javaClass.isInterface))
      case Some(remaining) =>
        val localClasses = classOwners
          .flatMap(cls => collectLocalClasses(cls, localClassName, None))
          .flatMap(_.classSymbol)
        localClasses.flatMap(s => findClassRecursively(s, remaining))

  private def findClassRecursively(owner: DeclaringSymbol, decodedName: String): Seq[DecodedClass.ClassDef] =
    owner.declarations
      .collect { case sym: ClassSymbol => sym }
      .flatMap { sym =>
        val Symbol = s"${Regex.quote(sym.sourceName)}\\$$?(.*)".r
        decodedName match
          case Symbol(remaining) =>
            if remaining.isEmpty then Some(DecodedClass.ClassDef(sym))
            else findClassRecursively(sym, remaining)
          case _ => None
      }

  private def collectLocalClasses(
      classSymbol: ClassSymbol,
      name: String,
      sourceLines: Option[binary.SourceLines]
  ): Seq[DecodedClass] =
    val localClasses = collectLiftedTrees(classSymbol, sourceLines) {
      case cls: LocalClass if cls.symbol.sourceName == name => cls
    }
      .map(cls => wrapIfInline(cls, DecodedClass.ClassDef(cls.symbol)))
    val samAndPartialFunctions = collectLiftedTrees(classSymbol, sourceLines) { case lambda: LambdaTree => lambda }
      .map { lambda =>
        val (term, samClass) = lambda.symbol
        wrapIfInline(lambda, DecodedClass.SAMOrPartialFunction(term, samClass, lambda.tpe.asInstanceOf))
      }
    localClasses ++ samAndPartialFunctions

  private def matchParents(
      decodedClass: DecodedClass,
      expectedParents: Set[ClassSymbol],
      isInterface: Boolean
  ): Boolean =
    decodedClass match
      case cls: DecodedClass.ClassDef =>
        val symbol = cls.symbol
        if symbol.isEnum then expectedParents == symbol.parentClasses.toSet + defn.ProductClass
        else if isInterface then expectedParents == symbol.parentClasses.filter(_.isTrait).toSet
        else if symbol.isAnonClass then symbol.parentClasses.forall(expectedParents.contains)
        else expectedParents == symbol.parentClasses.toSet
      case _: DecodedClass.SyntheticCompanionClass => false
      case anonFun: DecodedClass.SAMOrPartialFunction =>
        if anonFun.parentClass == defn.PartialFunctionClass then
          expectedParents == Set(defn.AbstractPartialFunctionClass, defn.SerializableClass)
        else expectedParents.contains(anonFun.parentClass)
      case inlined: DecodedClass.InlinedClass => matchParents(inlined.underlying, expectedParents, isInterface)

  private def matchParents(classSymbol: ClassSymbol, expectedParents: Set[ClassSymbol], isInterface: Boolean): Boolean =
    if classSymbol.isEnum then expectedParents == classSymbol.parentClasses.toSet + defn.ProductClass
    else if isInterface then expectedParents == classSymbol.parentClasses.filter(_.isTrait).toSet
    else if classSymbol.isAnonClass then classSymbol.parentClasses.forall(expectedParents.contains)
    else expectedParents == classSymbol.parentClasses.toSet

  private def wrapIfInline(liftedTree: LiftedTree[?], decodedClass: DecodedClass): DecodedClass =
    liftedTree match
      case InlinedFromDef(underlying, inlineCall) =>
        DecodedClass.InlinedClass(wrapIfInline(underlying, decodedClass), inlineCall.callTree)
      case _ => decodedClass

  private def findStaticJavaMethods(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    decodedClass.companionClassSymbol.toSeq
      .flatMap(_.declarations)
      .collect {
        case sym: TermSymbol
            if matchTargetName(method, sym) && matchSignature(method, sym.declaredType, checkParamNames = false) =>
          DecodedMethod.ValOrDefDef(decodedClass, sym)
      }

  private def findStandardMethods(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    def rec(underlying: DecodedClass): Seq[DecodedMethod] =
      underlying match
        case anonFun: DecodedClass.SAMOrPartialFunction =>
          if method.isConstructor then Seq(DecodedMethod.SAMOrPartialFunctionConstructor(decodedClass, anonFun.tpe))
          else if anonFun.parentClass == defn.PartialFunctionClass then
            Seq(findPartialFunctionImpl(decodedClass, anonFun.tpe, method))
          else findSAMFunctionImpl(decodedClass, anonFun.symbol, anonFun.parentClass, method).toSeq
        case underlying: DecodedClass.ClassDef => findInstanceMethods(decodedClass, underlying.symbol, method)
        case _: DecodedClass.SyntheticCompanionClass => Seq.empty
        case inlined: DecodedClass.InlinedClass => rec(inlined.underlying)
    rec(decodedClass)

  private def findParamForwarder(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod.ValOrDefDef] =
    decodedClass.declarations.collect {
      case sym: TermSymbol if names.contains(sym.targetNameStr) && matchSignature(method, sym.declaredType) =>
        DecodedMethod.ValOrDefDef(decodedClass, sym)
    }

  private def findTraitSetter(
      decodedClass: DecodedClass,
      method: binary.Method,
      name: String
  ): Seq[DecodedMethod.SetterAccessor] =
    for
      traitSym <- decodedClass.linearization.filter(_.isTrait)
      if method.decodedName.contains("$" + traitSym.nameStr + "$")
      sym <- traitSym.declarations.collect {
        case sym: TermSymbol if sym.targetNameStr == name && !sym.isMethod && !sym.isAbstractMember => sym
      }
      paramType <- decodedClass.thisType.map(sym.typeAsSeenFrom).collect { case tpe: Type => tpe }
    yield
      val tpe = MethodType(List(SimpleName("x$1")), List(paramType), defn.UnitType)
      DecodedMethod.SetterAccessor(decodedClass, sym, tpe)

  private def findSetter(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod.SetterAccessor] =
    for
      param <- method.allParameters.lastOption.toSeq
      sym <- findFieldSymbols(decodedClass, param.`type`, names)
    yield
      val tpe = MethodType(List(SimpleName("x$1")), List(sym.declaredType.asInstanceOf[Type]), defn.UnitType)
      DecodedMethod.SetterAccessor(decodedClass, sym, tpe)

  private def findFieldSymbols(
      decodedClass: DecodedClass,
      binaryType: binary.Type,
      names: Seq[String]
  ): Seq[TermSymbol] =
    def matchType0(sym: TermSymbol): Boolean = matchSetterArgType(sym.declaredType, binaryType)
    decodedClass.declarations.collect {
      case sym: TermSymbol if !sym.isMethod && names.exists(sym.targetNameStr == _) && matchType0(sym) =>
        sym
    }

  private def findSuperAccessor(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod] =
    for
      traitSym <- decodedClass.linearization.filter(_.isTrait)
      if method.decodedName.contains("$" + traitSym.nameStr + "$")
      sym <- traitSym.declarations.collect {
        case sym: TermSymbol if names.contains(sym.targetNameStr) && !sym.isAbstractMember => sym
      }
      expectedTpe <- decodedClass.thisType.map(sym.typeAsSeenFrom(_))
      if matchSignature(method, expectedTpe)
    yield DecodedMethod.SuperAccessor(decodedClass, sym, expectedTpe)

  private def findSpecializedMethod(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod.SpecializedMethod] =
    decodedClass.declarations.collect {
      case sym: TermSymbol
          if names.contains(sym.targetNameStr) &&
            matchSignature(
              method,
              sym.declaredType,
              captureAllowed = false,
              checkParamNames = false,
              checkTypeErasure = false
            ) &&
            // hack: in Scala 3 only overriding symbols can be specialized (Function and Tuple)
            sym.allOverriddenSymbols.nonEmpty =>
        DecodedMethod.SpecializedMethod(decodedClass, sym)
    }

  private def findInlineAccessor(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod] =
    val methodAccessors = method.instructions
      .collect { case binary.Instruction.Method(_, owner, name, descriptor, _) =>
        classLoader.loadClass(owner).method(name, descriptor)
      }
      .singleOpt
      .flatten
      .map { binaryTarget =>
        val target = decodeMethod(binaryTarget)
        // val tpe = target.declaredType.asSeenFrom(fromType, fromClass)
        DecodedMethod.InlineAccessor(decodedClass, target)
      }
    def singleFieldInstruction(f: binary.Instruction.Field => Boolean) = method.instructions
      .collect { case instr: binary.Instruction.Field => instr }
      .singleOpt
      .filter(f)
      .toSeq
    def fieldSetters =
      val expectedNames = names.map(_.stripSuffix("_=")).distinct
      for
        instr <- singleFieldInstruction(f => f.isPut && f.unexpandedDecodedNames.exists(expectedNames.contains))
        binaryField <- classLoader.loadClass(instr.owner).declaredField(instr.name).toSeq
        fieldOwner = decodeClass(binaryField.declaringClass)
        sym <- findFieldSymbols(fieldOwner, binaryField.`type`, instr.unexpandedDecodedNames)
      yield
        val tpe = MethodType(List(SimpleName("x$1")), List(sym.declaredType.asInstanceOf[Type]), defn.UnitType)
        val decodedTarget = DecodedMethod.SetterAccessor(fieldOwner, sym, tpe)
        DecodedMethod.InlineAccessor(decodedClass, decodedTarget)
    def fieldGetters =
      for
        instr <- singleFieldInstruction(f => !f.isPut && f.unexpandedDecodedNames.exists(names.contains))
        binaryField <- classLoader.loadClass(instr.owner).declaredField(instr.name).toSeq
        fieldOwner = decodeClass(binaryField.declaringClass)
        sym <- findFieldSymbols(fieldOwner, binaryField.`type`, instr.unexpandedDecodedNames)
      yield DecodedMethod.InlineAccessor(decodedClass, DecodedMethod.ValOrDefDef(fieldOwner, sym))
    def moduleAccessors =
      for
        instr <- singleFieldInstruction(_.name == "MODULE$")
        targetClass = decodeClass(classLoader.loadClass(instr.owner))
        targetClassSym <- targetClass.classSymbol
        targetTermSym <- targetClassSym.moduleValue
      yield DecodedMethod.InlineAccessor(decodedClass, DecodedMethod.ValOrDefDef(targetClass, targetTermSym))
    def valueClassAccessors =
      if method.instructions.isEmpty && method.isExtensionMethod then
        for
          companionClass <- decodedClass.companionClass.toSeq
          param <- method.allParameters.lastOption.toSeq
          field <- findFieldSymbols(companionClass, param.`type`, names.map(_.stripSuffix("$extension")))
        yield
          val decodedTarget = DecodedMethod.ValOrDefDef(decodedClass, field)
          DecodedMethod.InlineAccessor(decodedClass, decodedTarget)
      else Seq.empty
    methodAccessors.toSeq
      .orIfEmpty(fieldSetters)
      .orIfEmpty(fieldGetters)
      .orIfEmpty(moduleAccessors.toSeq)
      .orIfEmpty(valueClassAccessors)

  private def findInstanceMethods(
      decodedClass: DecodedClass,
      classSymbol: ClassSymbol,
      method: binary.Method
  ): Seq[DecodedMethod] =
    if method.isConstructor && classSymbol.isSubClass(defn.AnyValClass) then
      classSymbol.getAllOverloadedDecls(SimpleName("<init>")).map(DecodedMethod.ValOrDefDef(decodedClass, _))
    else
      val isJava = decodedClass.isJava
      val fromClass = classSymbol.declarations
        .collect { case sym: TermSymbol if matchTargetName(method, sym) => sym }
        .collect {
          case sym
              if matchSignature(
                method,
                sym.declaredType,
                asJavaVarargs = isJava,
                captureAllowed = !isJava,
                checkParamNames = !isJava
              ) =>
            DecodedMethod.ValOrDefDef(decodedClass, sym)
          case sym if !isJava && matchSignature(method, sym.declaredType, asJavaVarargs = true) =>
            DecodedMethod.Bridge(DecodedMethod.ValOrDefDef(decodedClass, sym), sym.declaredType)
        }
      fromClass.orIfEmpty(findAccessorsFromTraits(decodedClass, classSymbol, method))

  private def findAccessorsFromTraits(
      decodedClass: DecodedClass,
      classSymbol: ClassSymbol,
      method: binary.Method
  ): Seq[DecodedMethod] =
    if classSymbol.isTrait then Seq.empty
    else findAccessorsFromTraits(decodedClass, classSymbol, classSymbol.thisType, method)

  private def findAccessorsFromTraits(
      decodedClass: DecodedClass,
      fromClass: ClassSymbol,
      fromType: Type,
      method: binary.Method
  ): Seq[DecodedMethod] =
    for
      traitSym <- fromClass.linearization.filter(_.isTrait)
      if !method.isExpanded || method.decodedName.contains("$" + traitSym.nameStr + "$")
      sym <- traitSym.declarations
        .collect {
          case sym: TermSymbol if matchTargetName(method, sym) && matchSignature(method, sym.declaredType) => sym
        }
      if method.isExpanded == sym.isPrivate
      if sym.isParamAccessor || sym.isSetter || !sym.isMethod
      if sym.isOverridingSymbol(fromClass)
    yield
      val tpe = sym.typeAsSeenFrom(fromType)
      if sym.isParamAccessor then DecodedMethod.TraitParamAccessor(decodedClass, sym)
      else if sym.isSetter then DecodedMethod.SetterAccessor(decodedClass, sym, tpe)
      else DecodedMethod.GetterAccessor(decodedClass, sym, tpe)

  private def findLazyInit(decodedClass: DecodedClass, name: String): Seq[DecodedMethod] =
    val matcher: PartialFunction[Symbol, TermSymbol] =
      case sym: TermSymbol if sym.isModuleOrLazyVal && sym.nameStr == name => sym
    val fromClass = decodedClass.declarations.collect(matcher).map(DecodedMethod.LazyInit(decodedClass, _))
    def fromTraits =
      for
        traitSym <- decodedClass.linearization.filter(_.isTrait)
        term <- traitSym.declarations.collect(matcher)
        if term.isOverridingSymbol(decodedClass)
      yield DecodedMethod.LazyInit(decodedClass, term)
    fromClass.orIfEmpty(fromTraits)

  private def findTraitStaticForwarder(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Option[DecodedMethod.TraitStaticForwarder] =
    method.instructions
      .collect {
        case binary.Instruction.Method(_, owner, name, descriptor, _) if owner == method.declaringClass.name =>
          method.declaringClass.method(name, descriptor)
      }
      .singleOpt
      .flatten
      .map(target => DecodedMethod.TraitStaticForwarder(decodeMethod(decodedClass, target)))

  private def findOuter(decodedClass: DecodedClass): Option[DecodedMethod.OuterAccessor] =
    def outerClass(sym: Symbol): ClassSymbol = if sym.owner.isClass then sym.owner.asClass else outerClass(sym.owner)
    decodedClass.symbolOpt
      .map(outerClass)
      .map(outerClass => DecodedMethod.OuterAccessor(decodedClass, outerClass.thisType))

  private def findTraitInitializer(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod.ValOrDefDef] =
    decodedClass.declarations.collect {
      case sym: TermSymbol if sym.name == nme.Constructor => DecodedMethod.ValOrDefDef(decodedClass, sym)
    }

  private def findValueClassExtension(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Seq[DecodedMethod.ValOrDefDef] =
    val names = method.unexpandedDecodedNames.map(_.stripSuffix("$extension"))
    decodedClass.companionClassSymbol.toSeq.flatMap(_.declarations).collect {
      case sym: TermSymbol if names.contains(sym.targetNameStr) && matchSignature(method, sym.declaredType) =>
        DecodedMethod.ValOrDefDef(decodedClass, sym)
    }

  private def findStaticForwarder(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Seq[DecodedMethod.StaticForwarder] =
    decodedClass.companionClassSymbol.toSeq.flatMap(findStaticForwarder(decodedClass, _, method))

  private def findStaticForwarder(
      decodedClass: DecodedClass,
      companionObject: ClassSymbol,
      method: binary.Method
  ): Seq[DecodedMethod.StaticForwarder] =
    method.instructions
      .collect { case binary.Instruction.Method(_, owner, name, descriptor, _) =>
        classLoader.loadClass(owner).method(name, descriptor)
      }
      .flatten
      .singleOpt
      .toSeq
      .map(decodeMethod)
      .collect {
        case mixin: DecodedMethod.MixinForwarder => mixin.target
        case target => target
      }
      .map { target =>
        val declaredType = target.symbolOpt
          .map(_.typeAsSeenFrom(companionObject.thisType))
          .getOrElse(target.declaredType)
        DecodedMethod.StaticForwarder(decodedClass, target, declaredType)
      }

  private def findSAMFunctionImpl(
      decodedClass: DecodedClass,
      symbol: TermSymbol,
      parentClass: ClassSymbol,
      method: binary.Method
  ): Option[DecodedMethod] =
    val types =
      for
        parentCls <- parentClass.linearization.iterator
        overridden <- parentCls.declarations.collect { case term: TermSymbol if matchTargetName(method, term) => term }
        if overridden.overridingSymbol(parentClass).exists(_.isAbstractMember)
      yield DecodedMethod.SAMOrPartialFunctionImpl(decodedClass, overridden, symbol.declaredType)
    types.nextOption

  private def findPartialFunctionImpl(decodedClass: DecodedClass, tpe: Type, method: binary.Method): DecodedMethod =
    val implementedSym = defn.PartialFunctionClass.findNonOverloadedDecl(SimpleName(method.name))
    val implTpe = implementedSym.typeAsSeenFrom(SkolemType(tpe))
    DecodedMethod.SAMOrPartialFunctionImpl(decodedClass, implementedSym, implTpe)

  private def findBridgesAndMixinForwarders(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Option[DecodedMethod] =
    def rec(underlying: DecodedClass): Option[DecodedMethod] =
      underlying match
        case underlying: DecodedClass.ClassDef =>
          if !underlying.symbol.isTrait then
            findBridgesAndMixinForwarders(decodedClass, underlying.symbol, underlying.symbol.thisType, method)
          else None
        case underlying: DecodedClass.SAMOrPartialFunction =>
          findBridgesAndMixinForwarders(decodedClass, underlying.parentClass, SkolemType(underlying.tpe), method)
        case underlying: DecodedClass.InlinedClass => rec(underlying.underlying)
        case _: DecodedClass.SyntheticCompanionClass => None
    rec(decodedClass)

  private def findBridgesAndMixinForwarders(
      decodedClass: DecodedClass,
      fromClass: ClassSymbol,
      fromType: Type,
      method: binary.Method
  ): Option[DecodedMethod] =
    findBridges(decodedClass, fromClass, fromType, method)
      .orIfEmpty(findMixinForwarder(decodedClass, method))

  private def findBridges(
      decodedClass: DecodedClass,
      fromClass: ClassSymbol,
      fromType: Type,
      method: binary.Method
  ): Option[DecodedMethod] =
    method.instructions
      .collect {
        case binary.Instruction.Method(_, owner, name, descriptor, _) if name == method.name =>
          classLoader.loadClass(owner).method(name, descriptor)
      }
      .singleOpt
      .flatten
      .map { binaryTarget =>
        val target = decodeMethod(binaryTarget)
        val tpe = target.declaredType.asSeenFrom(fromType, fromClass)
        DecodedMethod.Bridge(target, tpe)
      }

  private def findMixinForwarder(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Option[DecodedMethod.MixinForwarder] =
    method.instructions
      .collect { case binary.Instruction.Method(_, owner, name, descriptor, _) =>
        classLoader.loadClass(owner).method(name, descriptor)
      }
      .singleOpt
      .flatten
      .filter(target => target.isStatic && target.declaringClass.isInterface)
      .map(decodeMethod)
      .collect { case staticForwarder: DecodedMethod.TraitStaticForwarder =>
        DecodedMethod.MixinForwarder(decodedClass, staticForwarder.target)
      }

  private def withCompanionIfExtendsAnyVal(cls: ClassSymbol): Seq[ClassSymbol] =
    cls.companionClass match
      case Some(companionClass) if companionClass.isSubClass(defn.AnyValClass) =>
        Seq(cls, companionClass)
      case _ => Seq(cls)

  private def findAdaptedAnonFun(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    if method.instructions.nonEmpty then
      val underlying = method.instructions
        .collect {
          case binary.Instruction.Method(_, owner, name, descriptor, _) if owner == method.declaringClass.name =>
            method.declaringClass.declaredMethod(name, descriptor)
        }
        .flatten
        .singleOrElse(unexpected(s"$method is not an adapted method: cannot find underlying invocation"))
      findAnonFunsAndByNameArgs(decodedClass, underlying).map(DecodedMethod.AdaptedFun(_))
    else Seq.empty

  private def findAnonFunsAndReduceAmbiguity(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Seq[DecodedMethod] =
    val candidates = findAnonFunsAndByNameArgs(decodedClass, method)
    if candidates.size > 1 then
      val clashingMethods = method.declaringClass.declaredMethods
        .filter(m => m.returnType.zip(method.returnType).forall(_ == _) && m.signature.name != method.signature.name)
        .collect { case m @ Patterns.AnonFun(_) if m.name != method.name => m }
        .map(m => m -> findAnonFunsAndByNameArgs(decodedClass, m).toSet)
        .toMap
      def reduceAmbiguity(
          methods: Map[binary.Method, Set[DecodedMethod]]
      ): Map[binary.Method, Set[DecodedMethod]] =
        val found = methods.collect { case (m, syms) if syms.size == 1 => syms.head }
        val reduced = methods.map { case (m, candidates) =>
          if candidates.size > 1 then m -> (candidates -- found)
          else m -> candidates
        }
        if reduced.count { case (_, s) => s.size == 1 } == found.size then methods
        else reduceAmbiguity(reduced)
      reduceAmbiguity(clashingMethods + (method -> candidates.toSet))(method).toSeq
    else candidates

  private def findAnonFunsAndByNameArgs(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Seq[DecodedMethod] =
    val anonFuns = findLocalMethods(decodedClass, method, Seq(CommonNames.anonFun.toString))
    val byNameArgs =
      if method.allParameters.forall(_.isCapture) then findByNameArgs(decodedClass, method)
      else Seq.empty
    reduceAmbiguityOnMethods(anonFuns ++ byNameArgs)

  private def findLocalMethods(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod] =
    collectLocalMethods(decodedClass, method) {
      case fun if names.contains(fun.symbol.name.toString) && matchLiftedFunSignature(method, fun) =>
        wrapIfInline(fun, DecodedMethod.ValOrDefDef(decodedClass, fun.symbol.asTerm))
    }

  private def reduceAmbiguityOnMethods(syms: Seq[DecodedMethod]): Seq[DecodedMethod] =
    if syms.size > 1 then
      val reduced = syms.filterNot(sym => syms.exists(enclose(sym, _)))
      if reduced.size != 0 then reduced else syms
    else syms

  private def enclose(enclosing: DecodedMethod, enclosed: DecodedMethod): Boolean =
    (enclosing, enclosed) match
      case (enclosing: DecodedMethod.InlinedMethod, enclosed: DecodedMethod.InlinedMethod) =>
        enclosing.callPos.enclose(enclosed.callPos) || (
          !enclosed.callPos.enclose(enclosing.callPos) &&
            enclose(enclosing.underlying, enclosed.underlying)
        )
      case (enclosing: DecodedMethod.InlinedMethod, enclosed) =>
        enclosing.callPos.enclose(enclosed.pos)
      case (enclosing, enclosed: DecodedMethod.InlinedMethod) =>
        enclosing.pos.enclose(enclosed.callPos)
      case (enclosing, enclosed) =>
        enclosing.pos.enclose(enclosed.pos)

  private def isInlineMethodApply(tree: Tree): Boolean =
    tree match
      case tree: TermReferenceTree if tree.symbol.isInline => true
      case Apply(fun, _) => isInlineMethodApply(fun)
      case TypeApply(fun, _) => isInlineMethodApply(fun)
      case _ => false

  private def findByNameArgs(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    collectLiftedTrees(decodedClass, method) { case arg: ByNameArg if !arg.isInline => arg }
      .collect {
        case arg if matchReturnType(arg.tpe, method.returnType) && matchCapture(arg.capture, method.allParameters) =>
          wrapIfInline(arg, DecodedMethod.ByNameArg(decodedClass, arg.owner, arg.tree, arg.tpe.asInstanceOf))
      }

  private def findByNameArgsProxy(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    val explicitByNameArgs =
      collectLiftedTrees(decodedClass, method) { case arg: ByNameArg if arg.isInline => arg }
        .collect {
          case arg if matchReturnType(arg.tpe, method.returnType) && matchCapture(arg.capture, method.allParameters) =>
            wrapIfInline(arg, DecodedMethod.ByNameArg(decodedClass, arg.owner, arg.tree, arg.tpe.asInstanceOf))
        }
    val inlineOverrides =
      for
        classSym <- decodedClass.classSymbol.toSeq
        sym <- classSym.declarations.collect {
          case sym: TermSymbol if sym.allOverriddenSymbols.nonEmpty && sym.isInline => sym
        }
        if method.sourceLines.forall(sym.pos.matchLines)
        paramSym <- sym.paramSymbols
        resultType <- Seq(paramSym.declaredType).collect { case tpe: ByNameType => tpe.resultType }
        if matchReturnType(resultType, method.returnType)
      yield
        val argTree = Ident(paramSym.name)(paramSym.localRef)(SourcePosition.NoPosition)
        DecodedMethod.ByNameArg(decodedClass, sym, argTree, resultType)
    explicitByNameArgs ++ inlineOverrides

  private def collectLocalMethods(
      decodedClass: DecodedClass,
      method: binary.Method
  )(
      matcher: PartialFunction[LiftedTree[TermSymbol], DecodedMethod]
  ): Seq[DecodedMethod] =
    collectLiftedTrees(decodedClass, method) { case term: LocalTermDef => term }
      .collect(matcher)

  private def findSuperArgs(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod.SuperConstructorArg] =
    def matchSuperArg(liftedArg: LiftedTree[Nothing]): Boolean =
      val primaryConstructor = liftedArg.owner.asClass.getAllOverloadedDecls(nme.Constructor).head
      // a super arg takes the same parameters as its constructor
      val scalaParams = prepareScalaParams(method, primaryConstructor.declaredType)
      val javaParams = prepareJavaParams(method, scalaParams)
      matchReturnType(liftedArg.tpe, method.returnType) && matchCapture(liftedArg.capture, javaParams.capturedParams)
    collectLiftedTrees(decodedClass, method) { case arg: ConstructorArg => arg }
      .collect {
        case liftedArg if matchSuperArg(liftedArg) =>
          DecodedMethod.SuperConstructorArg(
            decodedClass,
            liftedArg.owner.asClass,
            liftedArg.tree,
            liftedArg.tpe.asInstanceOf
          )
      }

  private def findLiftedTries(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    collectLiftedTrees(decodedClass, method) { case tree: LiftedTry => tree }
      .collect {
        case liftedTry if matchReturnType(liftedTry.tpe, method.returnType) =>
          wrapIfInline(
            liftedTry,
            DecodedMethod.LiftedTry(decodedClass, liftedTry.owner, liftedTry.tree, liftedTry.tpe.asInstanceOf)
          )
      }

  private def wrapIfInline(liftedTree: LiftedTree[?], decodedMethod: DecodedMethod): DecodedMethod =
    liftedTree match
      case InlinedFromDef(liftedTree, inlineCall) =>
        DecodedMethod.InlinedMethod(wrapIfInline(liftedTree, decodedMethod), inlineCall.callTree)
      case _ => decodedMethod

  private def collectLiftedTrees[S](decodedClass: DecodedClass, method: binary.Method)(
      matcher: PartialFunction[LiftedTree[?], LiftedTree[S]]
  ): Seq[LiftedTree[S]] =
    def withCompanionIfExtendsAnyVal(decodedClass: DecodedClass): Seq[Symbol] = decodedClass match
      case classDef: DecodedClass.ClassDef =>
        Seq(classDef.symbol) ++ classDef.symbol.companionClass.filter(_.isSubClass(defn.AnyValClass))
      case _: DecodedClass.SyntheticCompanionClass => Seq.empty
      case anonFun: DecodedClass.SAMOrPartialFunction => Seq(anonFun.symbol)
      case inlined: DecodedClass.InlinedClass => withCompanionIfExtendsAnyVal(inlined.underlying)

    val owners = withCompanionIfExtendsAnyVal(decodedClass)
    val sourceLines =
      if owners.size == 2 && method.allParameters.exists(p => p.name.matches("\\$this\\$\\d+")) then
        // workaround of https://github.com/lampepfl/dotty/issues/18816
        method.sourceLines.map(_.last)
      else method.sourceLines
    owners.flatMap(collectLiftedTrees(_, sourceLines)(matcher))

  private def collectLiftedTrees[S](owner: Symbol, sourceLines: Option[binary.SourceLines])(
      matcher: PartialFunction[LiftedTree[?], LiftedTree[S]]
  ): Seq[LiftedTree[S]] =
    LiftedTreeCollector.collect(owner)(matcher).filter(tree => sourceLines.forall(matchLines(tree, _)))

  private def matchLines(liftedFun: LiftedTree[?], sourceLines: binary.SourceLines): Boolean =
    val positions = liftedFun.positions.filter(pos => pos.sourceFile.name == sourceLines.sourceName)
    sourceLines.tastyLines.forall(line => positions.exists(_.containsLine(line)))

  private def matchTargetName(method: binary.Method, symbol: TermSymbol): Boolean =
    method.unexpandedDecodedNames.map(_.stripSuffix("$")).contains(symbol.targetNameStr)

  private case class ScalaParams(
      declaredParamNames: Seq[UnsignedTermName],
      declaredParamTypes: Seq[Type],
      expandedParamTypes: Seq[Type],
      returnType: Type
  ):
    def regularParamTypes: Seq[Type] = declaredParamTypes ++ expandedParamTypes

  private case class JavaParams(
      capturedParams: Seq[binary.Parameter],
      declaredParams: Seq[binary.Parameter],
      expandedParams: Seq[binary.Parameter],
      returnType: Option[binary.Type]
  ):
    def regularParams = declaredParams ++ expandedParams

  private def matchLiftedFunSignature(method: binary.Method, tree: LiftedTree[TermSymbol]): Boolean =
    val scalaParams = prepareScalaParams(method, tree.tpe)
    val javaParams = prepareJavaParams(method, scalaParams)

    def matchParamNames: Boolean =
      scalaParams.declaredParamNames
        .corresponds(javaParams.declaredParams)((name, javaParam) => name.toString == javaParam.name)

    def matchTypeErasure: Boolean =
      scalaParams.regularParamTypes
        .corresponds(javaParams.regularParams)((tpe, javaParam) => matchArgType(tpe, javaParam.`type`, false)) &&
        matchReturnType(scalaParams.returnType, javaParams.returnType)

    matchParamNames && matchTypeErasure && matchCapture(tree.capture, javaParams.capturedParams)
  end matchLiftedFunSignature

  private def matchCapture(capture: Seq[String], capturedParams: Seq[binary.Parameter]): Boolean =
    val anonymousPattern = "\\$\\d+".r
    val evidencePattern = "evidence\\$\\d+".r
    def toPattern(variable: String): Regex =
      variable match
        case anonymousPattern() => "\\$\\d+\\$\\$\\d+".r
        case evidencePattern() => "evidence\\$\\d+\\$\\d+".r
        case _ =>
          val encoded = NameTransformer.encode(variable)
          s"${Regex.quote(encoded)}(\\$$tailLocal\\d+)?(\\$$lzy\\d+)?\\$$\\d+".r
    val patterns = capture.map(toPattern)
    def isCapture(param: String) =
      patterns.exists(_.unapplySeq(param).nonEmpty)
    def isProxy(param: String) = "(.+)\\$proxy\\d+\\$\\d+".r.unapplySeq(param).nonEmpty
    def isThisOrOuter(param: String) = "(.+_|\\$)(this|outer)\\$\\d+".r.unapplySeq(param).nonEmpty
    def isLazy(param: String) = "(.+)\\$lzy\\d+\\$\\d+".r.unapplySeq(param).nonEmpty
    capturedParams.forall(p => isProxy(p.name) || isCapture(p.name) || isThisOrOuter(p.name) || isLazy(p.name))

  private def matchSignature(
      method: binary.Method,
      declaredType: TypeOrMethodic,
      expandContextFunction: Boolean = true,
      captureAllowed: Boolean = true,
      asJavaVarargs: Boolean = false,
      checkParamNames: Boolean = true,
      checkTypeErasure: Boolean = true
  ): Boolean =
    val scalaParams = prepareScalaParams(method, declaredType)
    val javaParams = prepareJavaParams(method, scalaParams)
    if !captureAllowed && javaParams.capturedParams.nonEmpty then false
    else
      def matchParamNames: Boolean =
        scalaParams.declaredParamNames
          .corresponds(javaParams.declaredParams)((name, javaParam) => name.toString == javaParam.name)

      def matchTypeErasure: Boolean =
        scalaParams.regularParamTypes
          .corresponds(javaParams.regularParams)((tpe, javaParam) =>
            matchArgType(tpe, javaParam.`type`, asJavaVarargs)
          ) && matchReturnType(scalaParams.returnType, javaParams.returnType)

      javaParams.capturedParams.forall(_.isGenerated) && // captures are generated
      javaParams.expandedParams.forall(_.isGenerated) && // expanded params are generated
      scalaParams.regularParamTypes.size == javaParams.regularParams.size &&
      (!checkParamNames || matchParamNames) &&
      (!checkTypeErasure || matchTypeErasure)
  end matchSignature

  private def prepareScalaParams(method: binary.Method, tpe: TermType): ScalaParams =
    val (expandedParamTypes, returnType) =
      if method.isConstructor && method.declaringClass.isJavaLangEnum then
        (List(defn.StringType, defn.IntType), tpe.returnType)
      else if !method.isAnonFun then expandContextFunctions(tpe.returnType, acc = Nil)
      else (List.empty, tpe.returnType)
    ScalaParams(tpe.allParamNames, tpe.allParamTypes, expandedParamTypes, returnType)

  /* After code generation, a method ends up with more than its declared parameters.
   *
   * It has, in order:
   * - capture params,
   * - declared params,
   * - "expanded" params (from java.lang.Enum constructors and uncurried context function types).
   *
   * We can only check the names of declared params.
   * We can check the (erased) type of declared and expand params; together we call them "regular" params.
   */
  private def prepareJavaParams(method: binary.Method, scalaParams: ScalaParams): JavaParams =
    val (capturedParams, regularParams) =
      method.allParameters.splitAt(method.allParameters.size - scalaParams.regularParamTypes.size)
    val (declaredParams, expandedParams) = regularParams.splitAt(scalaParams.declaredParamTypes.size)
    JavaParams(capturedParams, declaredParams, expandedParams, method.returnType)

  private def expandContextFunctions(tpe: Type, acc: List[Type]): (List[Type], Type) =
    tpe.dealias match
      case tpe: AppliedType if tpe.tycon.isContextFunction =>
        val argsAsTypes = tpe.args.map(_.highIfWildcard)
        expandContextFunctions(argsAsTypes.last, acc ::: argsAsTypes.init)
      case _ => (acc, tpe)

  private lazy val scalaPrimitivesToJava: Map[ClassSymbol, String] = Map(
    defn.BooleanClass -> "boolean",
    defn.ByteClass -> "byte",
    defn.CharClass -> "char",
    defn.DoubleClass -> "double",
    defn.FloatClass -> "float",
    defn.IntClass -> "int",
    defn.LongClass -> "long",
    defn.ShortClass -> "short",
    defn.UnitClass -> "void",
    defn.NullClass -> "scala.runtime.Null$"
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
