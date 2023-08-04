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
    val localMethodCounter = new Counter[Method]()
    val anonFunCounter = new Counter[Method]()

    val jars = TestingResolver.fetch("org.scala-lang", "scala3-compiler_3", "3.3.0")
    val unpickler = new Scala3Unpickler(jars.map(_.absolutePath).toArray ++ javaRuntimeJars, println, testMode = true)

    for
      cls <- loadClasses(jars, "scala3-compiler_3-3.3.0")
      clsSym <- cls match
        case LocalClass(_, _, _) => processLocalClass(unpickler, cls, localClassCounter)
        case _ => processClass(unpickler, cls)
        // case AnonClass(_, _, _) => process(cls, anonClassCounter)
        // case InnerClass(_, _) => process(cls, innerClassCounter)
        // case _ => process(cls, topLevelClassCounter)
      method <- cls.declaredMethods
      methSym <- method match

        case AnonFun(_) => processMethod(unpickler, method, anonFunCounter)
        // case LocalMethod(_, _) => processMethod(unpickler, method, localMethodCounter)
        // case LocalLazyInit(_, _, _) => process(method, localClassCounter)
        case _ => None
    do ()
    localClassCounter.printStatus("Local classes")
    localMethodCounter.printStatus("Local methods")
    anonFunCounter.printStatus("anon fun")

    val (noPrefix, others) = anonFunCounter.ambiguous.partition(_.name.startsWith("$anonfun"))
    println(s"noPrefix: ${noPrefix.size}")
    others.foreach(x => println(x.declaringClass.name + " " + x.name))

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

  def processLocalClass(unpickler: Scala3Unpickler, cls: ClassType, counter: Counter[ClassType]): Option[ClassSymbol] =
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
      case _ =>
        None

  def processClass(unpickler: Scala3Unpickler, cls: ClassType): Option[ClassSymbol] =
    try
      val sym = unpickler.findClass(cls)
      Some(sym)
    catch
      case _ =>
        None

  def processMethod(unpickler: Scala3Unpickler, mthd: Method, counter: Counter[Method]): Option[TermSymbol] =
    try
      val sym = unpickler.findSymbol(mthd)
      sym match
        case Some(t) =>
          counter.addSuccess(mthd)
          sym
        case None =>
          // println(mthd.name)
          // println(mthd.declaringClass.name)
          counter.addNotFound(mthd)
          None
    catch
      case AmbiguousException(e) =>
        // println(mthd.name)
        // println(mthd.declaringClass.name)
        counter.addAmbiguous(mthd)
        None
      case _ =>
        None

  class Counter[T]:
    val success: mutable.Buffer[T] = mutable.Buffer.empty[T]
    var notFound: mutable.Buffer[T] = mutable.Buffer.empty[T]
    var ambiguous: mutable.Buffer[T] = mutable.Buffer.empty[T]

    def addSuccess(cls: T) = success += cls

    def addNotFound(cls: T) = notFound += cls

    def addAmbiguous(cls: T) = ambiguous += cls

    def printStatus(m: String) =
      println(s"Status $m:")
      println(s"  - total is ${ambiguous.size + notFound.size + success.size}")
      println(s"  - success is ${success.size}")
      println(s"  - ambiguous is ${ambiguous.size}")
      println(s"  - notFound is ${notFound.size}")
