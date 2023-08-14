package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Names.*
import tastyquery.Types.*
import tastyquery.Modifiers.*

extension (symbol: Symbol)
  def isTrait = symbol.isClass && symbol.asClass.isTrait
  def isAnonFun = symbol.nameStr == "$anonfun"
  def isAnonClass = symbol.nameStr == "$anon"
  def matchName(name: String) =
    symbol.nameStr == name
  def isLocal = symbol.owner.isTerm
  def isModuleClass = symbol.isClass && symbol.asClass.isModuleClass
  def nameStr = symbol.name.toString

extension (symbol: TermSymbol)
  private def isGetterOrSetter = !symbol.isMethod || symbol.isSetter
  private def isLazyValInTrait: Boolean = symbol.owner.isTrait && symbol.isLazyVal
  private def isLazyVal: Boolean = symbol.kind == TermSymbolKind.LazyVal

extension [T](binarySymbols: Seq[T])
  def singleOrThrow(binaryName: String): T =
    singleOptOrThrow(binaryName)
      .getOrElse(throw new NotFoundException(s"Cannot find Scala symbol of $binaryName"))

  def singleOptOrThrow(binaryName: String): Option[T] =
    if binarySymbols.size > 1 then
      throw new AmbiguousException(s"Found ${binarySymbols.size} matching symbols for $binaryName")
    else binarySymbols.headOption

extension (name: Name)
  def isPackageObject: Boolean =
    val nameStr = name.toString
    nameStr == "package" || nameStr.endsWith("$package")

extension (tpe: Type)
  def isFunction: Boolean =
    tpe match
      case ref: TypeRef =>
        isScalaPackage(ref.prefix) && ref.nameStr.startsWith("Function")
      case _ => false

  def isTuple: Boolean =
    tpe match
      case ref: TypeRef =>
        isScalaPackage(ref.prefix) && ref.nameStr.startsWith("Tuple")
      case _ => false
  def isVarArg: Boolean =
    tpe match
      case ref: TypeRef =>
        isScalaPackage(ref.prefix) && ref.nameStr == "<repeated>"
      case _ => false

  def isOperatorLike: Boolean =
    tpe match
      case ref: TypeRef =>
        val operatorChars = "\\+\\-\\*\\/\\%\\&\\|\\^\\<\\>\\=\\!\\~\\#\\:\\@\\?"
        val regex = s"[^$operatorChars]".r
        !regex.findFirstIn(ref.nameStr).isDefined
      case _ => false

extension (tpe: NamedType) def nameStr: String = tpe.name.toString

extension (ref: TermRef)
  def isScalaPredef: Boolean =
    isScalaPackage(ref.prefix) && ref.nameStr == "Predef"

extension (prefix: Prefix)
  def isScalaPackage: Boolean =
    prefix match
      case p: PackageRef => p.fullyQualifiedName.toString == "scala"
      case _ => false

extension (tree: Tree)
  def walkTreeWithFilter[R](pred: Tree => Boolean)(op: Tree => R)(reduce: (R, R) => R, default: => R): R =
    // Apply the operation to the tree itself and all its sutbrees. Reduce the result with the given @reduce function
    def rec(t: Tree): R = reduce(op(t), t.subtrees.filter(pred).map(rec).foldLeft(default)(reduce))
    if pred(tree) then rec(tree) else default

  def subtrees: List[Tree] = tree match
    case PackageDef(pid, stats) => stats
    case ImportSelector(imported, renamed, bound) => imported :: renamed.toList ::: bound.toList
    case Import(expr, selectors) => expr :: selectors
    case Export(expr, selectors) => expr :: selectors
    case ClassDef(name, rhs, symbol) => rhs :: Nil
    case TypeMember(name, rhs, symbol) => rhs :: Nil
    case TypeParam(name, bounds, symbol) => bounds :: Nil
    case Template(constr, parents, self, body) => constr :: parents ::: self.toList ::: body
    case ValDef(name, tpt, rhs, symbol) => tpt :: rhs.toList
    case SelfDef(name, tpt) => tpt :: Nil
    case DefDef(name, params, tpt, rhs, symbol) => params.flatMap(_.merge) ::: tpt :: rhs.toList
    case Select(qualifier, name) => qualifier :: Nil
    case SelectOuter(qualifier, levels) => qualifier :: Nil
    case Super(qual, mix) => qual :: Nil
    case Apply(fun, args) => fun :: args
    case TypeApply(fun, args) => fun :: args
    case New(tpt) => tpt :: Nil
    case Typed(expr, tpt) => expr :: tpt :: Nil
    case Assign(lhs, rhs) => lhs :: rhs :: Nil
    case NamedArg(name, arg) => arg :: Nil
    case Block(stats, expr) => stats :+ expr
    case If(cond, thenPart, elsePart) => cond :: thenPart :: elsePart :: Nil
    case InlineIf(cond, thenPart, elsePart) => cond :: thenPart :: elsePart :: Nil
    case Lambda(meth, tpt) => meth :: tpt.toList
    case Match(selector, cases) => selector :: cases
    case InlineMatch(selector, cases) => selector.toList ::: cases
    case CaseDef(pattern, guard, body) => pattern :: guard.toList ::: body :: Nil
    case TypeTest(body, tpt) => body :: tpt :: Nil
    case Bind(name, body, symbol) => body :: Nil
    case Alternative(trees) => trees
    case Unapply(fun, implicits, patterns) => fun :: implicits ++ patterns
    case ExprPattern(expr) => expr :: Nil
    case SeqLiteral(elems, elemtpt) => elems ::: elemtpt :: Nil
    case While(cond, body) => cond :: body :: Nil
    case Throw(expr) => expr :: Nil
    case Try(expr, cases, finalizer) => (expr :: cases) ::: finalizer.toList
    case Return(expr, from) => expr.toList
    case Inlined(expr, caller, bindings) => expr :: bindings

    case SingletonTypeTree(term) => term :: Nil
    case RefinedTypeTree(parent, refinements, classSym) => parent :: refinements
    case ByNameTypeTree(result) => result :: Nil
    case AppliedTypeTree(tycon, args) => tycon :: args
    case TypeWrapper(tp) => Nil
    case SelectTypeTree(qualifier, name) => qualifier :: Nil
    case TermRefTypeTree(qualifier, name) => qualifier :: Nil
    case AnnotatedTypeTree(tpt, annotation) => tpt :: annotation :: Nil
    case MatchTypeTree(bound, selector, cases) => bound :: selector :: cases
    case TypeCaseDef(pattern, body) => pattern :: body :: Nil
    case TypeTreeBind(name, body, symbol) => body :: Nil
    case WildcardTypeArgTree(bounds) => bounds :: Nil
    case TypeLambdaTree(tparams, body) => tparams ::: body :: Nil
    case TypeBindingsTree(bindings, body) => bindings ::: body :: Nil

    case InferredTypeBoundsTree(bounds) => Nil
    case ExplicitTypeBoundsTree(low, high) => low :: high :: Nil
    case TypeAliasDefinitionTree(alias) => alias :: Nil
    case OpaqueTypeAliasDefinitionTree(bounds, alias) => bounds :: alias :: Nil
    case PolyTypeDefinitionTree(tparams, body) => tparams ::: body :: Nil
    case NamedTypeBoundsTree(name, bounds) => Nil

    case _: ImportIdent | _: Ident | _: This | _: Literal | _: WildcardPattern | _: TypeIdent => Nil
