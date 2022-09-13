package ch.epfl.scala.debugadapter.internal.stepfilter

import tastyquery.Contexts.Context
import tastyquery.Contexts
import tastyquery.jdk.ClasspathLoaders
import tastyquery.jdk.ClasspathLoaders.FileKind
import tastyquery.ast.Names.*
import tastyquery.ast.Symbols.*
import java.util.function.Consumer
import java.nio.file.Path
import ch.epfl.scala.debugadapter.internal.jdi
import tastyquery.ast.Flags
import scala.util.matching.Regex
import scala.util.Try
import tastyquery.ast.TermSig
import tastyquery.ast.TypeLenSig
import tastyquery.ast.ParamSig
import tastyquery.ast.Types.*

class ScalaStepFilterBridge(
    classpaths: Array[Path],
    warnLogger: Consumer[String],
    testMode: Boolean
):
  private val classpath =
    ClasspathLoaders.read(
      classpaths.toList,
      Set(FileKind.Tasty, FileKind.Class)
    )
  private given ctx: Context = Contexts.init(classpath)

  private def warn(msg: String): Unit = warnLogger.accept(msg)

  def skipMethod(obj: Any): Boolean =
    val method = jdi.Method(obj)
    val isExtensionMethod = method.name.endsWith("$extension")
    val fqcn = method.declaringType.name
    val matchingSymbols =
      extractScalaTerms(fqcn, isExtensionMethod).filter(
        matchSymbol(method, _, isExtensionMethod)
      )

    if matchingSymbols.size > 1 then
      val builder = new java.lang.StringBuilder
      builder.append(
        s"Found ${matchingSymbols.size} matching symbols for $method:\n"
      )
      matchingSymbols.foreach(sym => builder.append(s"$sym\n"))
      if testMode then throw new Exception(builder.toString)
      else warn(builder.toString)

    matchingSymbols.forall(skip)

  private[stepfilter] def extractScalaTerms(
      fqcn: String,
      isExtensionMethod: Boolean
  ): Seq[RegularSymbol] =
    for
      declaringType <- findDeclaringType(fqcn, isExtensionMethod).toSeq
      term <- declaringType.declarations
        .collect { case sym: RegularSymbol if sym.isTerm => sym }
    yield term

  private def findDeclaringType(
      fqcn: String,
      isExtensionMethod: Boolean
  ): Option[DeclaringSymbol] =
    val javaParts = fqcn.split('.')
    val isObject = fqcn.endsWith("$")
    val packageNames = javaParts.dropRight(1).toList.map(SimpleName.apply)
    val packageSym =
      ctx.findSymbolFromRoot(packageNames).asInstanceOf[PackageClassSymbol]
    val className = javaParts.last
    def findRec(
        owner: DeclaringSymbol,
        encodedName: String
    ): Seq[DeclaringSymbol] =
      owner.declarations
        .collect { case sym: DeclaringSymbol => sym }
        .flatMap { sym =>
          val Symbol = s"${Regex.quote(sym.name.toString)}\\$$?(.*)".r
          encodedName match
            case Symbol(remaining) =>
              if remaining.isEmpty then Some(sym)
              else findRec(sym, remaining)
            case _ => None
        }
    val clsSymbols = findRec(packageSym, className)
    val obj = clsSymbols.filter(_.is(Flags.Module))
    val cls = clsSymbols.filter(!_.is(Flags.Module))
    assert(obj.size <= 1 && cls.size <= 1)
    if isObject && !isExtensionMethod then obj.headOption else cls.headOption

  private def matchSymbol(
      method: jdi.Method,
      symbol: Symbol,
      isExtensionMethod: Boolean
  ): Boolean =
    matchName(method.name, symbol.name.toString, isExtensionMethod) &&
      matchSignature(method, symbol, isExtensionMethod)

  def matchName(
      javaName: String,
      scalaName: String,
      isExtensionMethod: Boolean
  ): Boolean =
    if isExtensionMethod then scalaName == javaName.stripSuffix("$extension")
    else scalaName == javaName

  def matchSignature(
      method: jdi.Method,
      symbol: Symbol,
      isExtensionMethod: Boolean
  ): Boolean =
    val notSupported = unsupported(symbol.declaredType)
    if unsupported(symbol.declaredType) then true
    else
      symbol.signedName match
        case SignedName(_, sig, _) =>
          val javaArgs = method.arguments.headOption.map(_.name) match
            case Some("$this") if isExtensionMethod => method.arguments.tail
            case _ => method.arguments
          matchArguments(sig.paramsSig, javaArgs) &&
          method.returnType.forall(matchType(sig.resSig, _))
        case _ =>
          true // TODO compare symbol.declaredType

  private def unsupported(tpe: Type): Boolean = tpe match
    case m: MethodType =>
      m.paramTypes.exists(unsupported) || unsupported(m.resultType)
    case m: PolyType => unsupported(m.resType)
    case _: ExprType => false
    case AppliedType(tycon, targs) =>
      unsupported(tycon) || targs.exists(unsupported)
    case tpe: Symbolic =>
      tpe.resolveToSymbol match
        case cls: ClassSymbol => false
        case sym => unsupported(sym.declaredType)
    case tpe: TypeParamRef => unsupported(tpe.bounds.high)
    case AndType(_, _) | OrType(_, _) => true
    case WildcardTypeBounds(bounds) => unsupported(bounds.high)
    case BoundedType(bounds, NoType) => unsupported(bounds.high)
    case BoundedType(_, alias) => unsupported(alias)
    case tpe => false

  private def matchArguments(
      scalaArgs: Seq[ParamSig],
      javaArgs: Seq[jdi.LocalVariable]
  ): Boolean =
    scalaArgs
      .collect { case termSig: TermSig => termSig }
      .corresponds(javaArgs) { (scalaArg, javaArg) =>
        matchType(scalaArg.typ, javaArg.`type`)
      }

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

  private def matchType(scalaType: TypeName, javaType: jdi.Type): Boolean =
    def rec(scalaType: String, javaType: String): Boolean =
      scalaType.toString match
        case "scala.Any[]" =>
          javaType == "java.lang.Object[]" || javaType == "java.lang.Object"
        case s"$scalaType[]" => rec(scalaType, javaType.stripSuffix("[]"))
        case _ =>
          val regex = scalaType
            .split('.')
            .map(Regex.quote)
            .mkString("", "[\\.\\$]", "\\$?")
            .r
          javaToScala
            .get(scalaType)
            .map(_ == javaType)
            .getOrElse(regex.matches(javaType))
    rec(scalaType.toString, javaType.name)
    // println(s"match type: ${scalaType.toString} ${javaType.name} $res")

  private def skip(symbol: RegularSymbol): Boolean =
    val isNonLazyGetterOrSetter =
      (!symbol.flags.is(Flags.Method) || symbol.is(Flags.Accessor)) &&
        !symbol.is(Flags.Lazy)
    isNonLazyGetterOrSetter || symbol.is(Flags.Synthetic)
