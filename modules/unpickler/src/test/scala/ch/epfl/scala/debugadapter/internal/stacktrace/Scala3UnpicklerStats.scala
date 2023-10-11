package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.*
import ch.epfl.scala.debugadapter.internal.IO
import ch.epfl.scala.debugadapter.internal.binary
import ch.epfl.scala.debugadapter.internal.javareflect.*
import ch.epfl.scala.debugadapter.testfmk.TestingResolver
import org.objectweb.asm
import tastyquery.Symbols.*

import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.Path
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Properties
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationInt
import scala.util.control.NonFatal

class Scala3UnpicklerStats extends munit.FunSuite:
  private val javaRuntime = JavaRuntime(Properties.jdkHome).get
  private val javaRuntimeJars = javaRuntime match
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

    val jars = TestingResolver.fetch("org.scala-lang", "scala3-compiler_3", "3.3.0")
    val unpickler = new Scala3Unpickler(jars.map(_.absolutePath).toArray ++ javaRuntimeJars, println, testMode = true)

    for
      cls <- loadClasses(jars, "scala3-compiler_3-3.3.0")
      // if cls.name == "dotty.tools.dotc.typer.Deriving$Deriver"
      clsSym <- cls match
        case Patterns.LocalClass(_, _, _) => unpickler.tryFind(cls, localClassCounter)
        case Patterns.AnonClass(_, _) => unpickler.tryFind(cls, anonClassCounter)
        case Patterns.InnerClass(_) => unpickler.tryFind(cls, innerClassCounter)
        case _ => unpickler.tryFind(cls, topLevelClassCounter)
      method <- cls.declaredMethodsAndConstructors
    // if method.name == "deriveSingleParameter$1"
    do
      method match
        case Patterns.AnonFun(_) => unpickler.tryFind(method, anonFunCounter)
        case Patterns.AdaptedAnonFun(_) => unpickler.tryFind(method, adaptedAnonFunCounter)
        case Patterns.LocalLazyInit(_, _) => unpickler.tryFind(method, localLazyInitCounter)
        case Patterns.LocalMethod(_, _) => unpickler.tryFind(method, localMethodCounter)
        case _ => unpickler.tryFind(method, methodCounter)
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
    checkCounter(localMethodCounter, 2585, expectedAmbiguous = 2, expectedNotFound = 3)
    checkCounter(anonFunCounter, 6649, expectedAmbiguous = 331, expectedNotFound = 5)
    checkCounter(adaptedAnonFunCounter, 286, expectedAmbiguous = 83, expectedNotFound = 2)
    checkCounter(localLazyInitCounter, 107)
    checkCounter(methodCounter, 47097, expectedAmbiguous = 161, expectedNotFound = 10633)

  def checkCounter(
      counter: Counter,
      expectedSuccess: Int,
      expectedAmbiguous: Int = 0,
      expectedNotFound: Int = 0,
      expectedExceptions: Int = 0
  )(using munit.Location): Unit =
    assertEquals(counter.success.size, expectedSuccess)
    assertEquals(counter.ambiguous.size, expectedAmbiguous)
    assertEquals(counter.notFound.size, expectedNotFound)
    assertEquals(counter.exceptions.size, expectedExceptions)

  def loadClasses(jars: Seq[Library], jarName: String): Seq[JavaReflectClass] =
    val jar = jars.find(_.name == jarName).get
    val classNames = IO
      .withinJarFile(jar.absolutePath) { fs =>
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
    val classLoader = new URLClassLoader(jars.map(_.absolutePath.toUri.toURL).toArray)
    val loader = JavaReflectLoader(classLoader)
    val classes = classNames.map(loader.loadClass)
    println(s"Loaded ${classes.size} classes in $jarName.")
    classes

  extension (unpickler: Scala3Unpickler)
    def tryFind(cls: binary.ClassType, counter: Counter): Option[BinaryClassSymbol] =
      try
        val sym = unpickler.findClass(cls)
        counter.success += cls
        Some(sym)
      catch
        case ambiguious: AmbiguousException =>
          counter.ambiguous += ambiguious
          None
        case notFound: NotFoundException =>
          counter.notFound += notFound
          None
        case e: Exception =>
          counter.exceptions += (cls -> e)
          None

    def tryFind(mthd: binary.Method, counter: Counter): Unit =
      try
        val sym = unpickler.findMethod(mthd)
        counter.success += mthd
      catch
        case notFound: NotFoundException => counter.notFound += notFound
        case ambiguous: AmbiguousException => counter.ambiguous += ambiguous
        case e: Exception => counter.exceptions += (mthd -> e)

  override def munitTimeout: Duration = 2.minutes

  class Counter(name: String):
    val success = mutable.Buffer.empty[binary.Symbol]
    val notFound = mutable.Buffer.empty[NotFoundException]
    val ambiguous = mutable.Buffer.empty[AmbiguousException]
    val exceptions = mutable.Buffer.empty[(binary.Symbol, Exception)]

    def size: Int = success.size + notFound.size + ambiguous.size + exceptions.size

    def printReport() =
      def format(kind: String, count: Int): Option[String] =
        val percent = count * 100 / size
        Option.when(count > 0)(s"$kind: $count ($percent%)")
      if size > 0 then
        val stats = Seq(
          "success" -> success.size,
          "ambiguous" -> ambiguous.size,
          "not found" -> notFound.size,
          "exceptions" -> exceptions.size
        )
          .flatMap(format)
          .map("\n  - " + _)
          .mkString
        println(s"$size $name: $stats")

    def printNotFound() =
      notFound.foreach { case NotFoundException(symbol) =>
        val lines = symbol.sourceLines.interval.mkString("(", ", ", ")")
        println(s"$symbol $lines not found")
      }

    def printAmbiguous() =
      ambiguous.foreach { case AmbiguousException(symbol, candidates) =>
        val lines = symbol.sourceLines.interval.mkString("(", ", ", ")")
        println(s"$symbol $lines is ambiguous:" + candidates.map(s"\n  - " + _).mkString)
      }

    def printFirstException() = exceptions.headOption.foreach(printException)
    def printExceptions() = exceptions.foreach((sym, e) => println(s"$sym $e"))

    private def printException(sym: binary.Symbol, e: Exception) =
      println(s"$sym $e")
      e.printStackTrace()
