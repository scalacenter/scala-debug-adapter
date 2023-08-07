package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.*
import ch.epfl.scala.debugadapter.internal.IO
import ch.epfl.scala.debugadapter.internal.binary.*
import ch.epfl.scala.debugadapter.internal.javareflect.*
import ch.epfl.scala.debugadapter.internal.stacktrace.LocalClass
import ch.epfl.scala.debugadapter.testfmk.TestingResolver
import org.objectweb.asm.ClassReader
import tastyquery.Symbols.*

import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.Path
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Properties

class Scala3UnpicklerStats extends munit.FunSuite:
  private val javaRuntime = JavaRuntime(Properties.jdkHome).get
  private val javaRuntimeJars = javaRuntime match
    case Java8(_, classJars, _) => classJars
    case java9OrAbove: Java9OrAbove =>
      java9OrAbove.classSystems.map(_.fileSystem.getPath("/modules", "java.base"))

  test("dotty stats"):
    val localClassCounter = new Counter[ClassType]()
    val topLevelOrInnerclassCounter = new Counter[ClassType]()
    val localMethodCounter = new Counter[Method]()
    val anonFunCounter = new Counter[Method]()

    val jars = TestingResolver.fetch("org.scala-lang", "scala3-compiler_3", "3.3.0")
    val unpickler = new Scala3Unpickler(jars.map(_.absolutePath).toArray ++ javaRuntimeJars, println, testMode = true)

    for
      cls <- loadClasses(jars, "scala3-compiler_3-3.3.0")
      clsSym <- cls match
        case LocalClass(_, _, _) => processClass(unpickler, cls, localClassCounter)
        case _ => processClass(unpickler, cls, topLevelOrInnerclassCounter)
        // case AnonClass(_, _, _) => process(cls, anonClassCounter)
        // case InnerClass(_, _) => process(cls, innerClassCounter)
        // case _ => process(cls, topLevelClassCounter)
      method <- cls.declaredMethods
      methSym <- method match
        case AnonFun(_) => processMethod(unpickler, method, anonFunCounter)
        case LocalMethod(_) => processMethod(unpickler, method, localMethodCounter)
        case _ => None
        // case LocalLazyInit(_, _, _) => process(method, localClassCounter)
    do ()
    localClassCounter.printStatus("Local classes")
    localMethodCounter.printStatus("Local methods")
    anonFunCounter.printStatus("anon fun")
    topLevelOrInnerclassCounter.printStatus("topLevelOrInnerClass")

  def loadClasses(jars: Seq[Library], jarName: String) =
    val jar = jars.find(_.name == jarName).get
    val classLoader = new URLClassLoader(jars.map(_.absolutePath.toUri.toURL).toArray)
    val classes = IO
      .withinJarFile(jar.absolutePath) { fs =>
        val root = fs.getPath("/")
        val sourceMatcher = fs.getPathMatcher("glob:**.class")
        Files
          .walk(root: Path)
          .filter(sourceMatcher.matches)
          .iterator
          .asScala
          .map { classFile =>
            val inputStream = Files.newInputStream(classFile)
            val reader = new ClassReader(inputStream)
            reader.getClassName.replace('/', '.')
          }
          .toSeq
      }
      .get
      .map(name => JavaReflectClass(classLoader.loadClass(name)))
    println(s"classNames: ${classes.size}")
    classes

  def processClass(unpickler: Scala3Unpickler, cls: ClassType, counter: Counter[ClassType]): Option[ClassSymbol] =
    try
      val sym = unpickler.findClass(cls)
      counter.addSuccess(cls)
      Some(sym)
    catch
      case AmbiguousException(e) =>
        counter.addAmbiguous(cls)
        None
      case NotFoundException(e) =>
        counter.addNotFound(cls)
        None
      case e =>
        counter.exceptions += e.toString
        None

  def processMethod(unpickler: Scala3Unpickler, mthd: Method, counter: Counter[Method]): Option[TermSymbol] =
    try
      val sym = unpickler.findSymbol(mthd)
      sym match
        case Some(t) =>
          counter.addSuccess(mthd)
          sym
        case None =>
          counter.addNotFound(mthd)
          None
    catch
      case AmbiguousException(e) =>
        counter.addAmbiguous(mthd)
        None
      case e =>
        counter.exceptions += e.toString
        None

  class Counter[T]:
    val success: mutable.Buffer[T] = mutable.Buffer.empty[T]
    var notFound: mutable.Buffer[T] = mutable.Buffer.empty[T]
    var ambiguous: mutable.Buffer[T] = mutable.Buffer.empty[T]
    var exceptions: mutable.Buffer[String] = mutable.Buffer.empty[String]

    def addSuccess(cls: T) = success += cls

    def addNotFound(cls: T) = notFound += cls

    def addAmbiguous(cls: T) = ambiguous += cls

    def printStatus(m: String) =
      println(s"Status $m:")
      println(s"  - total is ${ambiguous.size + notFound.size + success.size}")
      println(s"  - success is ${success.size}")
      println(s"  - ambiguous is ${ambiguous.size}")
      println(s"  - notFound is ${notFound.size}")
      println(s"  - exceptions is ${exceptions.size}")
