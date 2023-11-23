package ch.epfl.scala.debugadapter.internal.stacktrace

import scala.util.Properties

class BinaryDecoderStats extends BinaryDecoderStatsBase:

  test("scala3-compiler:3.3.1"):
    val decoder = initDecoder("org.scala-lang", "scala3-compiler_3", "3.3.1")
    decoder.assertDecode(
      "scala.quoted.runtime.impl.QuotesImpl",
      "boolean scala$quoted$runtime$impl$QuotesImpl$$inline$xCheckMacro()",
      "QuotesImpl.<inline QuotesImpl.xCheckMacro>: Boolean"
    )
    decoder.assertDecode(
      "dotty.tools.dotc.printing.RefinedPrinter",
      "void dotty$tools$dotc$printing$RefinedPrinter$$inline$myCtx_$eq(dotty.tools.dotc.core.Contexts$Context x$0)",
      "RefinedPrinter.<inline RefinedPrinter.myCtx_=>(Contexts.Context): Unit"
    )
    decoder.assertDecodeAll(
      expectedClasses = ExpectedCount(4426),
      expectedMethods = ExpectedCount(68453, ambiguous = 25, notFound = 1)
    )

  test("scala3-compiler:3.0.2"):
    val decoder = initDecoder("org.scala-lang", "scala3-compiler_3", "3.0.2")
    decoder.assertDecodeAll(
      expectedClasses = ExpectedCount(3859, notFound = 3),
      expectedMethods = ExpectedCount(60794, ambiguous = 24, notFound = 131)
    )

  test("de.sciss:desktop-core:0.11.4"):
    assume(clue(Properties.javaVersion) == "17")
    val decoder = initDecoder("de.sciss", "desktop-core_3", "0.11.4")
    // fixed by tastyquery#395
    decoder.assertDecode(
      "de.sciss.desktop.impl.LogPaneImpl$textPane$",
      "boolean apply$mcZD$sp(double x$0)",
      "LogPaneImpl.textPane.apply.<specialized>(str: String): Unit"
    )
    decoder.assertDecodeAll(
      expectedClasses = ExpectedCount(236),
      expectedMethods = ExpectedCount(2751, ambiguous = 2, notFound = 6)
    )

  test("io.github.vigoo:zio-aws-ec2_3:4.0.5 - slow".ignore):
    val decoder = initDecoder("io.github.vigoo", "zio-aws-ec2_3", "4.0.5")
    decoder.assertDecodeAll(
      ExpectedCount(8413, notFound = 9),
      ExpectedCount(157420, ambiguous = 6, notFound = 473)
    )
