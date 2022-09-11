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

class ScalaStepFilterBridge(
    classpaths: Array[Path],
    warnLogger: Consumer[String],
    testMode: Boolean
):
  val kinds = Set(FileKind.Tasty, FileKind.Class)
  val classpath =
    ClasspathLoaders.read(classpaths.toList, kinds)
  given ctx: Context = Contexts.init(classpath)

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

    matchingSymbols.headOption.forall(skip)

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
    symbol.signedName match
      case SignedName(_, sig, _) =>
        val javaArgs = method.arguments.headOption.map(_.name) match
          case Some("$this") if isExtensionMethod => method.arguments.tail
          case _ => method.arguments
        val scalaArgs = sig.paramsSig.filter(!_.isInstanceOf[TypeLenSig])
        matchArguments(scalaArgs, javaArgs) && matchType(
          sig.resSig,
          method.returnType
        )
      case _ =>
        true // TODO compare symbol.declaredType

  def matchArguments(
      scalaArgs: Seq[ParamSig],
      javaArgs: Seq[jdi.LocalVariable]
  ): Boolean =
    scalaArgs.corresponds(javaArgs) { (scalaArg, javaArg) =>
      scalaArg match
        case TermSig(typeName) => matchType(typeName, javaArg.`type`)
        case TypeLenSig(len) =>
          if (testMode) throw new Exception(s"Unexpected $TypeLenSig") else true
    }

  def matchType(scalaType: TypeName, javaType: jdi.Type): Boolean =
    val expectedType = scalaType.toString match
      case "java.lang.Boolean" | "scala.Boolean" => "boolean".r
      case "java.lang.Byte" | "scala.Byte" => "byte".r
      case "java.lang.Character" | "scala.Char" => "char".r
      case "java.lang.Double" | "scala.Double" => "double".r
      case "java.lang.Float" | "scala.Float" => "float".r
      case "java.lang.Integer" | "scala.Int" => "int".r
      case "java.lang.Long" | "scala.Long" => "long".r
      case "java.lang.Short" | "scala.Short" => "short".r
      case "scala.Unit" => "void".r
      case "scala.Any" => Regex.quote("java.lang.Object").r
      case scalaTypeName =>
        scalaTypeName
          .split('.')
          .map(Regex.quote)
          .mkString("", "[\\.\\$]", "\\$?")
          .r
    expectedType.matches(javaType.name)
    // println(s"match type: ${scalaType.toString} ${javaType.name}   (${expectedType.regex})")

  def skip(symbol: RegularSymbol): Boolean =
    val isNonLazyGetterOrSetter =
      (!symbol.flags.is(Flags.Method) || symbol.is(Flags.Accessor)) &&
        !symbol.is(Flags.Lazy)
    isNonLazyGetterOrSetter || symbol.is(Flags.Synthetic)
