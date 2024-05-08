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

class Scala3DecoderBridge(
    classEntries: Array[Path],
    warnLogger: Consumer[String],
    testMode: Boolean
):
  private val decoder: BinaryDecoder = BinaryDecoder.cached(classEntries)(
    // make it quiet, or it would be too verbose when things go wrong
    using ThrowOrWarn(_ => (), testMode)
  )
  private val formatter = StackTraceFormatter(using ThrowOrWarn(s => warnLogger.accept(s), testMode))

  def decode(obj: Any): DecodedMethodBridge =
    new DecodedMethodBridge(decoder.decode(JdiMethod(obj)), formatter)
