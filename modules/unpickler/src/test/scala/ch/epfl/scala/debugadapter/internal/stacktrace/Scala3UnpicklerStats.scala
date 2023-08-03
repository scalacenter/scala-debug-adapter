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

object Scala3UnpicklerStats:
  private val javaRuntime = JavaRuntime(Properties.jdkHome).get
  private val javaRuntimeJars = javaRuntime match
    case Java8(_, classJars, _) => classJars
    case java9OrAbove: Java9OrAbove =>
      java9OrAbove.classSystems.map(_.fileSystem.getPath("/modules", "java.base"))

  def main(args: Array[String]): Unit =

    val topLevelAndInnerClassCounter = new Counter()
    val localClassCounter = new Counter()
    val localMethodCounter = new Counter()

    val jars = TestingResolver.fetch("org.scala-lang", "scala3-compiler_3", "3.3.0")
    val unpickler = new Scala3Unpickler(jars.map(_.absolutePath).toArray ++ javaRuntimeJars, println, testMode = true)

    for
      cls <- loadClasses(jars, "scala3-compiler_3-3.3.0")
      clsSym <- cls match
        case LocalClass(_, _, _) => processClass(unpickler, cls, localClassCounter)
        case _ => processClass(unpickler, cls, topLevelAndInnerClassCounter)
        // case AnonClass(_, _, _) => process(cls, anonClassCounter)
        // case InnerClass(_, _) => process(cls, innerClassCounter)
        // case _ => process(cls, topLevelClassCounter)
      method <- cls.declaredMethods
      methSym <- method match
        case LocalMethod(_, _) => processMethod(unpickler, method, localMethodCounter)
        // case LocalLazyInit(_, _, _) => process(method, localClassCounter)
        case _ => None
    do ()
    localClassCounter.printStatus("Local classes")
    localMethodCounter.printStatus("Top level and inner classes")
    localMethodCounter.printStatus("Local methods")

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

  def processClass(unpickler: Scala3Unpickler, cls: ClassType, counter: Counter): Option[ClassSymbol] =
    try
      val sym = unpickler.findClass(cls)
      counter.addSuccess(cls.name)
      Some(sym)
    catch
      case AmbiguousException(e) =>
        counter.addAmbiguous(cls.name)
        None
      case NotFoundException(e) =>
        counter.addNotFound(cls.name)
        None
      case _ =>
        None

  def processMethod(unpickler: Scala3Unpickler, mthd: Method, counter: Counter): Option[TermSymbol] =
    try
      val sym = unpickler.findSymbol(mthd)
      sym match
        case Some(t) =>
          counter.addSuccess(mthd.name)
          sym
        case None =>
          counter.addNotFound(mthd.name)
          None
    catch
      case AmbiguousException(e) =>
        counter.addAmbiguous(mthd.name)
        None
      case _ =>
        None

  class Counter:
    val success: mutable.Set[String] = mutable.Set.empty[String]
    var notFound: mutable.Set[String] = mutable.Set.empty[String]
    var ambiguous: mutable.Set[String] = mutable.Set.empty[String]

    def addSuccess(cls: String) = success.add(cls)

    def addNotFound(cls: String) = notFound.add(cls)

    def addAmbiguous(cls: String) = ambiguous.add(cls)

    def printStatus(m: String) =
      println(s"Status $m:")
      println(s"  - total is ${ambiguous.size + notFound.size + success.size}")
      println(s"  - success is ${success.size}")
      println(s"  - ambiguous is ${ambiguous.size}")
      println(s"  - notFound is ${notFound.size}")
