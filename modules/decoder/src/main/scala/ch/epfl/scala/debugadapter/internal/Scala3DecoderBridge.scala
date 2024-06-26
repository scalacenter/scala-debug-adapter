package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.decoder.binary

import java.nio.file.Path
import java.util.function.Consumer
import ch.epfl.scala.decoder.*
import ch.epfl.scala.decoder.jdi.JdiMethod

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

  def decode(obj: com.sun.jdi.Method): DecodedMethodBridge =
    new DecodedMethodBridge(decoder.decode(JdiMethod(obj)), formatter)
