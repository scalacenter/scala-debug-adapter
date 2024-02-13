package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.internal.binary
import ch.epfl.scala.debugadapter.internal.jdi.JdiMethod
import ch.epfl.scala.debugadapter.internal.jdi.JdiClassLoader
import ch.epfl.scala.debugadapter.internal.stacktrace.*
import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Signatures.*
import tastyquery.SourcePosition
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.jdk.ClasspathLoaders

import java.nio.file.Path
import java.util.Optional
import java.util.function.Consumer
import scala.jdk.OptionConverters.*
import scala.util.matching.Regex

class Scala3DecoderBridge(
    classEntries: Array[Path],
    warnLogger: Consumer[String],
    testMode: Boolean
):
  val decoder: BinaryDecoder = BinaryDecoder(classEntries)(
    // make it quiet, or it would be too verbose when things go wrong
    using ThrowOrWarn(_ => (), testMode)
  )
  private val formatter = StackTraceFormatter(using ThrowOrWarn(s => warnLogger.accept(s), testMode))
  private val impl: Scala3Decoder = Scala3Decoder(formatter)

  def skipMethod(obj: Any): Boolean =
    val decodedMethod = decoder.decode(JdiMethod(obj))
    impl.skip(decodedMethod)

  def formatMethod(obj: Any): Optional[String] =
    val decodedMethod = decoder.decode(JdiMethod(obj))
    impl.format(decodedMethod).toJava

class Scala3Decoder(formatter: StackTraceFormatter):
  def skip(method: DecodedMethod): Boolean =
    method match
      case method: DecodedMethod.ValOrDefDef =>
        val sym = method.symbol
        (sym.isGetter && (!sym.owner.isTrait || !sym.isModuleOrLazyVal)) || // getter
        (sym.isLocal && sym.isModuleOrLazyVal) || // local def
        sym.isSetter ||
        (sym.isSynthetic && !sym.isLocal) ||
        sym.isExport
      case method: DecodedMethod.LazyInit => method.symbol.owner.isTrait
      case _: DecodedMethod.TraitStaticForwarder => true
      case _: DecodedMethod.TraitParamAccessor => true
      case _: DecodedMethod.MixinForwarder => true
      case _: DecodedMethod.Bridge => true
      case _: DecodedMethod.StaticForwarder => true
      case _: DecodedMethod.OuterAccessor => true
      case _: DecodedMethod.SetterAccessor => true
      case _: DecodedMethod.GetterAccessor => true
      case _: DecodedMethod.SuperAccessor => true
      case _: DecodedMethod.SpecializedMethod => true
      case _: DecodedMethod.InlineAccessor => true
      case _: DecodedMethod.AdaptedFun => true
      case _: DecodedMethod.SAMOrPartialFunctionConstructor => true
      case method: DecodedMethod.InlinedMethod => skip(method.underlying)
      case _ => false

  def format(method: DecodedMethod): Option[String] =
    def rec(method: DecodedMethod): Option[String] =
      method match
        case method: DecodedMethod.LazyInit if method.symbol.owner.isTrait => None
        case _: DecodedMethod.MixinForwarder => None
        case _: DecodedMethod.TraitStaticForwarder => None
        case _: DecodedMethod.Bridge => None
        case _: DecodedMethod.StaticForwarder => None
        case method: DecodedMethod.SetterAccessor if method.symbol.isVal => None
        case _: DecodedMethod.SuperAccessor => None
        case _: DecodedMethod.SpecializedMethod => None
        case _: DecodedMethod.InlineAccessor => None
        case _: DecodedMethod.AdaptedFun => None
        case method: DecodedMethod.InlinedMethod => rec(method.underlying)
        case m => Some(formatter.format(m))
    rec(method)
