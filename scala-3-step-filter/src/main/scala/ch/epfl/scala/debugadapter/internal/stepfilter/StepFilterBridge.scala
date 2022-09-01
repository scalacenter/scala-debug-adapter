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

class StepFilterBridge(
    classpaths: Array[Path],
    warnLogger: Consumer[String],
    testMode: Boolean
):
  val kinds = Set(FileKind.Tasty, FileKind.Class)
  val classpath =
    ClasspathLoaders.read(classpaths.map(_.toString).toList, kinds)
  given ctx: Context = Contexts.init(classpath)

  private def warn(msg: String): Unit = warnLogger.accept(msg)

  def skipMethod(obj: Any): Boolean =
    val method = jdi.Method(obj)
    val isExtensionMethod = method.name.endsWith("$extension")
    val fqcn = method.declaringType.name
    val matchingSymbols =
      extractScalaTerms(fqcn, isExtensionMethod).filter(matchSymbol(method, _))

    val builder = new java.lang.StringBuilder
    builder.append(
      s"Found ${matchingSymbols.size} matching symbols for $method:\n"
    )
    matchingSymbols.foreach(sym => builder.append(s"$sym\n"))

    if matchingSymbols.size > 1 then
      if testMode then throw new Exception(builder.toString)
      else warn(builder.toString)
    // else println(builder.toString)

    matchingSymbols.headOption.forall(skip)

  private[stepfilter] def extractScalaTerms(
      fqcn: String,
      isExtensionMethod: Boolean
  ): Seq[RegularSymbol] =
    for
      declaringType <- findDeclaringType(fqcn, isExtensionMethod).toSeq
      terms <- declaringType.declarations
        .collect { case sym: RegularSymbol if sym.isTerm => sym }
    yield terms

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
          val Symbol = s"${sym.name.toString}\\$$?(.*)".r
          encodedName match
            case Symbol(remaining) =>
              if remaining.isEmpty then Some(sym)
              else findRec(sym, remaining)
            case _ => None
        }

    val clsSymbols = findRec(packageSym, className)
    val obj = clsSymbols.filter(_.name.toTypeName.wrapsObjectName)
    val cls = clsSymbols.filter(!_.name.toTypeName.wrapsObjectName)
    assert(obj.size <= 1 && cls.size <= 1)
    if isObject && !isExtensionMethod then obj.headOption else cls.headOption

  private def matchSymbol(method: jdi.Method, symbol: Symbol): Boolean =
    matchName(method.name, symbol.name.toString)

  private def matchName(javaName: String, scalaName: String): Boolean =
    val scalaNameReg = s"$scalaName(\\$$extension)?".r
    scalaNameReg.matches(javaName)

  private def skip(symbol: RegularSymbol): Boolean =
    !symbol.is(Flags.Method) || symbol.is(Flags.Accessor)
