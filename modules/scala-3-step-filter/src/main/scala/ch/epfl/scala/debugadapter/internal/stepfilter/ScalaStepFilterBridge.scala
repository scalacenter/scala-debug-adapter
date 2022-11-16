package ch.epfl.scala.debugadapter.internal.stepfilter

import tastyquery.Contexts.Context
import tastyquery.Contexts
import tastyquery.jdk.ClasspathLoaders
import tastyquery.jdk.ClasspathLoaders.FileKind
import tastyquery.Names.*
import tastyquery.Symbols.*
import java.util.function.Consumer
import java.nio.file.Path
import ch.epfl.scala.debugadapter.internal.jdi
import tastyquery.Flags
import scala.util.matching.Regex
import scala.util.Try
import tastyquery.Types.*
import tastyquery.Signatures.*

class ScalaStepFilterBridge(
    classpaths: Array[Path],
    warnLogger: Consumer[String],
    testMode: Boolean
):
  private val classpath = ClasspathLoaders.read(classpaths.toList)
  private given ctx: Context = Contexts.init(classpath)

  private def warn(msg: String): Unit = warnLogger.accept(msg)

  private def throwOrWarn(msg: String): Unit =
    if (testMode) throw new Exception(msg)
    else warn(msg)

  def skipMethod(obj: Any): Boolean =
    val method = jdi.Method(obj)
    val isExtensionMethod = method.name.endsWith("$extension")
    val fqcn = method.declaringType.name
    findDeclaringType(fqcn, isExtensionMethod) match
      case None =>
        throwOrWarn(s"Cannot find Scala symbol of $fqcn")
        false
      case Some(declaringType) =>
        val matchingSymbols =
          declaringType.declarations
            .collect { case sym: TermSymbol if sym.isTerm => sym }
            .filter(matchSymbol(method, _, isExtensionMethod))

        if matchingSymbols.size > 1 then
          val builder = new java.lang.StringBuilder
          builder.append(
            s"Found ${matchingSymbols.size} matching symbols for $method:"
          )
          matchingSymbols.foreach(sym => builder.append(s"\n$sym"))
          throwOrWarn(builder.toString)

        matchingSymbols.forall(skip)

  private[stepfilter] def extractScalaTerms(
      fqcn: String,
      isExtensionMethod: Boolean
  ): Seq[TermSymbol] =
    for
      declaringType <- findDeclaringType(fqcn, isExtensionMethod).toSeq
      term <- declaringType.declarations
        .collect { case sym: TermSymbol if sym.isTerm => sym }
    yield term

  private def findDeclaringType(
      fqcn: String,
      isExtensionMethod: Boolean
  ): Option[DeclaringSymbol] =
    val javaParts = fqcn.split('.')
    val isObject = fqcn.endsWith("$")
    val packageNames = javaParts.dropRight(1).toList.map(SimpleName.apply)
    val packageSym =
      if packageNames.nonEmpty
      then ctx.findSymbolFromRoot(packageNames).asInstanceOf[PackageSymbol]
      else ctx.defn.EmptyPackage
    val className = javaParts.last
    def findRec(
        owner: DeclaringSymbol,
        encodedName: String
    ): Seq[DeclaringSymbol] =
      owner.declarations
        .collect { case sym: DeclaringSymbol => sym }
        .flatMap { sym =>
          val encodedSymName = NameTransformer.encode(sym.name.toString)
          val Symbol = s"${Regex.quote(encodedSymName)}\\$$?(.*)".r
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
      symbol: TermSymbol,
      isExtensionMethod: Boolean
  ): Boolean =
    matchName(method.name, symbol.name.toString, isExtensionMethod) &&
      matchSignature(method, symbol, isExtensionMethod)

  def matchName(
      javaName: String,
      scalaName: String,
      isExtensionMethod: Boolean
  ): Boolean =
    val encodedScalaName = NameTransformer.encode(scalaName)
    if isExtensionMethod then encodedScalaName == javaName.stripSuffix("$extension")
    else encodedScalaName == javaName

  def matchSignature(
      method: jdi.Method,
      symbol: TermSymbol,
      isExtensionMethod: Boolean
  ): Boolean =
    try {
      symbol.signedName match
        case SignedName(_, sig, _) =>
          val javaArgs = method.arguments.headOption.map(_.name) match
            case Some("$this") if isExtensionMethod => method.arguments.tail
            case _ => method.arguments
          matchArguments(sig.paramsSig, javaArgs) &&
          method.returnType.forall(matchType(sig.resSig, _))
        case _ =>
          true // TODO compare symbol.declaredType
    } catch {
      case e: UnsupportedOperationException =>
        warn(e.getMessage)
        true
    }

  private def matchArguments(
      scalaArgs: Seq[ParamSig],
      javaArgs: Seq[jdi.LocalVariable]
  ): Boolean =
    scalaArgs
      .collect { case termSig: ParamSig.Term => termSig }
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
