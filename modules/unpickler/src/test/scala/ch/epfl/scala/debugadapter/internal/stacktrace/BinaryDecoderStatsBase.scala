package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.*
import ch.epfl.scala.debugadapter.internal.IO
import ch.epfl.scala.debugadapter.internal.binary
import ch.epfl.scala.debugadapter.internal.javareflect.*
import ch.epfl.scala.debugadapter.testfmk.DebuggableFunSuite
import ch.epfl.scala.debugadapter.testfmk.TestingResolver
import tastyquery.Symbols.*

import java.nio.file.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Properties

trait BinaryDecoderStatsBase extends DebuggableFunSuite:
  private val formatter = StackTraceFormatter(println, testMode = true)
  private val javaRuntime = JavaRuntime(Properties.jdkHome).get

  def println(x: Any): Unit = Predef.println(x)

  def initDecoder(groupId: String, artifactId: String, version: String): TestingDecoder =
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
    def assertDecode(className: String, expected: String)(using munit.Location): Unit =
      val cls = decoder.classLoader.loadClass(className)
      val decodedClass = decoder.decodeClass(cls)
      assertEquals(formatter.format(decodedClass), expected)

    def assertDecode(className: String, javaSig: String, expected: String)(using munit.Location): Unit =
      val method = loadBinaryMethod(className, javaSig)
      val decodedMethod = decoder.decodeMethod(method)
      assertEquals(formatter.format(decodedMethod), expected)

    def assertDecodeAll(expectedClasses: ExpectedCount, expectedMethods: ExpectedCount, printProgress: Boolean = false)(
        using munit.Location
    ): Unit =
      val (classCounter, methodCounter) = decodeAll(printProgress)
      classCounter.check(expectedClasses)
      methodCounter.check(expectedMethods)

    def decodeAll(printProgress: Boolean = false): (Counter, Counter) =
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

  case class ExpectedCount(success: Int, ambiguous: Int = 0, notFound: Int = 0, throwables: Int = 0)

  case class Count(name: String, success: Int = 0, ambiguous: Int = 0, notFound: Int = 0, throwables: Int = 0):
    def size: Int = success + notFound + ambiguous + throwables
    def successPercent: Float = percent(success)

    def check(expected: ExpectedCount)(using munit.Location): Unit =
      assertEquals(success, expected.success)
      assertEquals(ambiguous, expected.ambiguous)
      assertEquals(notFound, expected.notFound)
      assertEquals(throwables, expected.throwables)

    def merge(count: Count): Count =
      Count(
        name,
        count.success + success,
        count.ambiguous + ambiguous,
        count.notFound + notFound,
        count.throwables + throwables
      )

    def printReport() =
      def format(kind: String, count: Int): Option[String] =
        Option.when(count > 0)(s"$kind: $count (${percent(count)}%)")
      if size > 0 then
        val stats =
          Seq("success" -> success, "ambiguous" -> ambiguous, "not found" -> notFound, "throwables" -> throwables)
            .flatMap(format)
            .map("\n  - " + _)
            .mkString
        println(s"$name ($size): $stats")

    private def percent(count: Int): Float = count.toFloat * 100 / size
  end Count

  class Counter(val name: String):
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
