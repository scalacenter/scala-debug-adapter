package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.*
import ch.epfl.scala.debugadapter.internal.IO
import ch.epfl.scala.debugadapter.internal.binary
import ch.epfl.scala.debugadapter.internal.javareflect.*
import ch.epfl.scala.debugadapter.testfmk.TestingResolver
import ch.epfl.scala.debugadapter.testfmk.DebuggableFunSuite
import tastyquery.Symbols.*

import java.nio.file.Files
import java.nio.file.Path
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Properties
import scala.util.control.NonFatal

class BinaryDecoderStats extends DebuggableFunSuite:
  private val javaRuntime = JavaRuntime(Properties.jdkHome).get match
    case Java8(_, classJars, _) => classJars
    case java9OrAbove: Java9OrAbove =>
      java9OrAbove.classSystems.flatMap { s =>
        Seq(
          s.fileSystem.getPath("/modules/java.base"),
          s.fileSystem.getPath("/modules/java.management")
        )
      }

  test("scala3-compiler:3.3.0"):
    val localClassCounter = Counter("local classes")
    val innerClassCounter = Counter("inner classes")
    val anonClassCounter = Counter("anon classes")
    val topLevelClassCounter = Counter("top-level classes")

    val localMethodCounter = Counter("local methods")
    val anonFunCounter = Counter("anon functions")
    val adaptedAnonFunCounter = Counter("adapted anon functions")
    val localLazyInitCounter = Counter("local lazy initializers")
    val methodCounter = Counter("methods")

    val libraries = TestingResolver.fetch("org.scala-lang", "scala3-compiler_3", "3.3.0")
    val decoder = BinaryDecoder(libraries.map(_.absolutePath), javaRuntime)

    for
      cls <- loadClasses(libraries, "scala3-compiler_3-3.3.0", decoder.classLoader)
      // if cls.name == "dotty.tools.dotc.cc.CaptureSet$"
      clsSym <- cls match
        case Patterns.LocalClass(_, _, _) => decoder.tryDecode(cls, localClassCounter)
        case Patterns.AnonClass(_, _) => decoder.tryDecode(cls, anonClassCounter)
        case Patterns.InnerClass(_) => decoder.tryDecode(cls, innerClassCounter)
        case _ => decoder.tryDecode(cls, topLevelClassCounter)
      method <- cls.declaredMethods
    // if method.name == "dotty$tools$dotc$cc$CaptureSet$$$Diff$superArg$1"
    do
      method match
        case Patterns.AnonFun(_) => decoder.tryDecode(method, anonFunCounter)
        case Patterns.AdaptedAnonFun(_) => decoder.tryDecode(method, adaptedAnonFunCounter)
        case Patterns.LocalLazyInit(_) => decoder.tryDecode(method, localLazyInitCounter)
        case Patterns.LocalMethod(_) => decoder.tryDecode(method, localMethodCounter)
        case _ => decoder.tryDecode(method, methodCounter)
    anonClassCounter.printFirstThrowable()
    localMethodCounter.printNotFound()
    anonFunCounter.printNotFound()
    methodCounter.printNotFound()
    localMethodCounter.printAmbiguous()
    // anonFunCounter.printAmbiguous()
    // anonFunCounter.printNotFound()
    localClassCounter.printReport()
    anonClassCounter.printReport()
    innerClassCounter.printReport()
    topLevelClassCounter.printReport()
    localMethodCounter.printReport()
    anonFunCounter.printReport()
    adaptedAnonFunCounter.printReport()
    localLazyInitCounter.printReport()
    methodCounter.printReport()
    checkCounter(localClassCounter, 42)
    checkCounter(anonClassCounter, 430)
    checkCounter(innerClassCounter, 2409)
    checkCounter(topLevelClassCounter, 1505)
    checkCounter(localMethodCounter, 2608)
    checkCounter(anonFunCounter, 6960, expectedAmbiguous = 24, expectedNotFound = 1)
    checkCounter(adaptedAnonFunCounter, 370, expectedAmbiguous = 1)
    checkCounter(localLazyInitCounter, 108)
    checkCounter(methodCounter, 57872)

  def checkCounter(
      counter: Counter,
      expectedSuccess: Int,
      expectedIgnored: Int = 0,
      expectedAmbiguous: Int = 0,
      expectedNotFound: Int = 0,
      expectedThrowables: Int = 0
  )(using munit.Location): Unit =
    assertEquals(counter.success.size, expectedSuccess)
    assertEquals(counter.ignored.size, expectedIgnored)
    assertEquals(counter.ambiguous.size, expectedAmbiguous)
    assertEquals(counter.notFound.size, expectedNotFound)
    assertEquals(counter.throwables.size, expectedThrowables)

  def loadClasses(
      libraries: Seq[Library],
      libraryName: String,
      binaryLoader: binary.BinaryClassLoader
  ): Seq[binary.ClassType] =
    val libary = libraries.find(_.name == libraryName).get
    val classNames = IO
      .withinJarFile(libary.absolutePath) { fs =>
        val classMatcher = fs.getPathMatcher("glob:**.class")
        Files
          .walk(fs.getPath("/"): Path)
          .filter(classMatcher.matches)
          .iterator
          .asScala
          .map(_.toString.stripPrefix("/").stripSuffix(".class").replace('/', '.'))
          .toSeq
      }
      .get
    val classes = classNames.map(binaryLoader.loadClass)
    println(s"Loaded ${classes.size} classes in $libraryName.")
    classes

  extension (decoder: BinaryDecoder)
    def tryDecode(cls: binary.ClassType, counter: Counter): Option[DecodedClass] =
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

    def tryDecode(mthd: binary.Method, counter: Counter): Unit =
      try
        val sym = decoder.decodeMethod(mthd)
        counter.success += mthd
      catch
        case notFound: NotFoundException => counter.notFound += (mthd -> notFound)
        case ambiguous: AmbiguousException => counter.ambiguous += ambiguous
        case ignored: IgnoredException => counter.ignored += ignored
        case e => counter.throwables += (mthd -> e)

  class Counter(name: String):
    val success = mutable.Buffer.empty[binary.Symbol]
    val notFound = mutable.Buffer.empty[(binary.Symbol, NotFoundException)]
    val ambiguous = mutable.Buffer.empty[AmbiguousException]
    val ignored = mutable.Buffer.empty[IgnoredException]
    val throwables = mutable.Buffer.empty[(binary.Symbol, Throwable)]

    def size: Int = success.size + notFound.size + ambiguous.size + ignored.size + throwables.size

    def printReport() =
      def format(kind: String, count: Int): Option[String] =
        val percent = count * 100 / size
        Option.when(count > 0)(s"$kind: $count ($percent%)")
      if size > 0 then
        val stats = Seq(
          "success" -> success.size,
          "ignored" -> ignored.size,
          "ambiguous" -> ambiguous.size,
          "not found" -> notFound.size,
          "throwables" -> throwables.size
        )
          .flatMap(format)
          .map("\n  - " + _)
          .mkString
        println(s"$size $name: $stats")

    def printNotFound() =
      notFound.foreach { case (s1, NotFoundException(s2)) =>
        if s1 != s2 then println(s"$s1 not found because of $s2")
        else println(s"$s1 not found")
      }

    def printAmbiguous() =
      ambiguous.foreach { case AmbiguousException(symbol, candidates) =>
        println(s"$symbol is ambiguous:" + candidates.map(s"\n  - " + _).mkString)
      }

    def printFirstThrowable() = throwables.headOption.foreach(printThrowable)
    def printThrowables() = throwables.foreach((sym, e) => println(s"$sym $e"))

    private def printThrowable(sym: binary.Symbol, e: Throwable) =
      println(s"$sym $e")
      e.printStackTrace()
