package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.*
import ch.epfl.scala.debugadapter.internal.IO
import ch.epfl.scala.debugadapter.internal.binary
import ch.epfl.scala.debugadapter.internal.javareflect.*
import ch.epfl.scala.debugadapter.testfmk.DebuggableFunSuite
import ch.epfl.scala.debugadapter.testfmk.TestingResolver
import tastyquery.Symbols.*

import java.io.PrintWriter
import java.nio.file.*
import scala.collection.mutable
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.Properties
import scala.util.Random
import scala.util.control.NonFatal

class BinaryDecoderStats extends DebuggableFunSuite:
  private val formatter = StackTraceFormatter(println, testMode = true)
  private val javaRuntime = JavaRuntime(Properties.jdkHome).get

  val file = Paths.get(s"test-result-${Random.nextInt(Int.MaxValue)}.txt")
  Predef.println(file)
  val pw = new PrintWriter(file.toFile)

  def println(s: Any): Unit =
    Predef.println(s)
    pw.println(s)
    pw.flush()

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

  test("de.sciss:desktop-core_3:0.11.4"):
    val decoder = initDecoder("de.sciss", "desktop-core_3", "0.11.4")
    decoder.assertDecode(
      "de.sciss.desktop.impl.LogPaneImpl",
      "int de$sciss$desktop$impl$LogPaneImpl$$textPane$$superArg$1()",
      ""
    )
    decoder.assertDecode(
      "de.sciss.desktop.impl.LogPaneImpl$textPane$",
      "boolean apply$mcZD$sp(double x$0)",
      "LogPaneImpl.textPane.<init>(): Unit"
    )
    decoder.assertDecodeAll(
      expectedClasses = ExpectedCount(229, throwables = 7),
      expectedMethods = ExpectedCount(2705, notFound = 6, throwables = 27)
    )

  test("io.github.vigoo:zio-aws-ec2_3:4.0.5"):
    val decoder = initDecoder("io.github.vigoo","zio-aws-ec2_3","4.0.5")
    decoder.assertDecodeAll(
      ExpectedCount(8413, notFound = 9),
      ExpectedCount(157420, ambiguous = 6, notFound = 473)
    )

  test("org.typelevel:cats-effect-testing-specs2_3:1.5.0"):
    val decoder = initDecoder("org.typelevel","cats-effect-testing-specs2_3","1.5.0")
    decoder.assertDecodeAll(
      expectedClasses = ExpectedCount(6),
      expectedMethods = ExpectedCount(62)
    )

  test("all Scala 3 ecosystem".only):
    assume(clue(Properties.javaVersion) == "17")
    assume(!isCI)
    val csv = Source.fromResource("scala3-artifacts-231121.csv")
    val classCounts = mutable.Buffer.empty[Count]
    val methodCounts = mutable.Buffer.empty[Count]
    for line <- csv.getLines.drop(1) do
      val parts = line.split(',').map(_.drop(1).dropRight(1))
      val (org, artifact, version) = (parts(0), parts(1), parts(2))
      try
        val decoder = initDecoder(org, artifact, version)
        val (classCounter, methodCounter) = decoder.decodeAll()
        classCounts += classCounter.count
        methodCounts += methodCounter.count
      catch case e => println(s"cannot decode $line")

    val totalClassCounter = classCounts.foldLeft(Count("total classes"))(_.merge(_))
    val totalMethodCounter = methodCounts.foldLeft(Count("total methods"))(_.merge(_))
    totalClassCounter.printReport()
    totalMethodCounter.printReport()

    (classCounts ++ methodCounts).toSeq
      .sortBy(count => - count.successPercent)
      .foreach(c => println(s"${c.name} ${c.successPercent}%"))

  private def initDecoder(groupId: String, artifactId: String, version: String): TestingDecoder =
    val libraries = TestingResolver.fetch(groupId, artifactId, version)
    val library = libraries.find(l => l.name.startsWith(artifactId.stripSuffix("_3")) && l.version == version).get
    val javaRuntimeJars = javaRuntime match
      case Java8(_, classJars, _) => classJars
      case java9OrAbove: Java9OrAbove =>
        java9OrAbove.classSystems.flatMap { s =>
          Files.list(s.fileSystem.getPath("/modules")).iterator.asScala.toSeq
        }
    val decoder = BinaryDecoder(libraries.map(_.absolutePath), javaRuntimeJars)
    TestingDecoder(library, decoder)

  extension (decoder: TestingDecoder)
    private def assertDecode(className: String, expected: String)(using munit.Location): Unit =
      val cls = decoder.classLoader.loadClass(className)
      val decodedClass = decoder.decodeClass(cls)
      assertEquals(formatter.format(decodedClass), expected)

    private def assertDecode(className: String, javaSig: String, expected: String)(using munit.Location): Unit =
      val method = loadBinaryMethod(className, javaSig)
      val decodedMethod = decoder.decodeMethod(method)
      assertEquals(formatter.format(decodedMethod), expected)

    private def assertDecodeAll(expectedClasses: ExpectedCount, expectedMethods: ExpectedCount, printProgress: Boolean = false)(using
        munit.Location
    ): Unit =
      val (classCounter, methodCounter) = decodeAll(printProgress)
      classCounter.check(expectedClasses)
      methodCounter.check(expectedMethods)

    private def decodeAll(printProgress: Boolean = false): (Counter, Counter) =
      val classCounter = Counter(decoder.name + " classes")
      val methodCounter = Counter(decoder.name + " methods")
      for
        binaryClass <- decoder.allClasses
        _ = if printProgress then println(s"\"${binaryClass.name}\"")
        decodedClass <- decoder.tryDecode(binaryClass, classCounter)
        binaryMethod <- binaryClass.declaredMethods
      do
        if printProgress then println(s"\"${binaryClass.name}\", \"${customFormat(binaryMethod)}\"")
        decoder.tryDecode(decodedClass, binaryMethod, methodCounter)
      classCounter.printReport()
      methodCounter.printReport()
      (classCounter, methodCounter)

    private def loadBinaryMethod(declaringType: String, javaSig: String)(using
        munit.Location
    ): binary.Method =
      val methods = decoder.classLoader.loadClass(declaringType).declaredMethods
      def notFoundMessage: String =
        s"Cannot find method '$javaSig':\n" + methods.map(m => s"  " + customFormat(m)).mkString("\n")
      methods.find(m => customFormat(m) == javaSig).getOrElse(throw new Exception(notFoundMessage))

    private def tryDecode(cls: binary.ClassType, counter: Counter): Option[DecodedClass] =
      try
        val sym = decoder.decodeClass(cls)
        counter.success += cls
        Some(sym)
      catch
        case ambiguious: AmbiguousException =>
          counter.ambiguous += ambiguious
          None
        case notFound: NotFoundException =>
          counter.notFound += (cls -> notFound)
          None
        case e =>
          counter.throwables += (cls -> e)
          None

    private def tryDecode(cls: DecodedClass, mthd: binary.Method, counter: Counter): Unit =
      try
        val sym = decoder.decodeMethod(cls, mthd)
        counter.success += mthd
      catch
        case notFound: NotFoundException => counter.notFound += (mthd -> notFound)
        case ambiguous: AmbiguousException => counter.ambiguous += ambiguous
        case ignored: IgnoredException => counter.ignored += ignored
        case e => counter.throwables += (mthd -> e)
  end extension

  private def customFormat(m: binary.Symbol): String =
    m match
      case m: binary.Method =>
        val returnType = m.returnType.map(_.name).get
        val parameters = m.allParameters.map(p => p.`type`.name + " " + p.name).mkString(", ")
        s"$returnType ${m.name}($parameters)"
      case m => m.toString

  private case class ExpectedCount(success: Int, ambiguous: Int = 0, notFound: Int = 0, throwables: Int = 0)

  private case class Count(name: String, success: Int = 0, ambiguous: Int = 0, notFound: Int = 0, throwables: Int = 0):
    def size: Int = success + notFound + ambiguous + throwables
    def successPercent: Float = percent(success)

    def check(expected: ExpectedCount)(using munit.Location): Unit =
      assertEquals(success, expected.success)
      assertEquals(ambiguous, expected.ambiguous)
      assertEquals(notFound, expected.notFound)
      assertEquals(throwables, expected.throwables)

    def merge(count: Count): Count =
      Count(name, count.success + success, count.ambiguous + ambiguous, count.notFound + notFound, count.throwables + throwables)

    def printReport() =
      def format(kind: String, count: Int): Option[String] =
        Option.when(count > 0)(s"$kind: $count (${percent(count)}%)")
      if size > 0 then
        val stats = Seq("success" -> success, "ambiguous" -> ambiguous, "not found" -> notFound, "throwables" -> throwables)
          .flatMap(format)
          .map("\n  - " + _)
          .mkString
        println(s"$name ($size): $stats")

    private def percent(count: Int): Float = count.toFloat * 100 / size
  end Count

  private class Counter(val name: String):
    val success = mutable.Buffer.empty[binary.Symbol]
    val notFound = mutable.Buffer.empty[(binary.Symbol, NotFoundException)]
    val ambiguous = mutable.Buffer.empty[AmbiguousException]
    val ignored = mutable.Buffer.empty[IgnoredException]
    val throwables = mutable.Buffer.empty[(binary.Symbol, Throwable)]

    def count: Count = Count(name, success.size, ambiguous.size, notFound.size, throwables.size)

    def printReport() = count.printReport()

    def printNotFound() =
      notFound.foreach { case (s1, NotFoundException(s2)) =>
        if s1 != s2 then println(s"${customFormat(s1)} not found because of ${customFormat(s2)}")
        else println(s"${customFormat(s1)} not found")
      }

    def printAmbiguous() =
      ambiguous.foreach { case AmbiguousException(s, candidates) =>
        println(s"${customFormat(s)} is ambiguous:" + candidates.map(s"\n  - " + _).mkString)
      }

    def printFirstThrowable() = throwables.headOption.foreach(printThrowable)

    def check(expected: ExpectedCount)(using munit.Location): Unit =
      assertEquals(success.size, expected.success)
      assertEquals(ambiguous.size, expected.ambiguous)
      assertEquals(notFound.size, expected.notFound)
      assertEquals(throwables.size, expected.throwables)

    private def printThrowable(sym: binary.Symbol, e: Throwable) =
      println(s"$sym $e")
      e.printStackTrace()
