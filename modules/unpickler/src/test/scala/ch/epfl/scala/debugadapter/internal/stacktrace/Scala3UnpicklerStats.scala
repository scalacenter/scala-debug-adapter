package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.*
import ch.epfl.scala.debugadapter.internal.IO
import ch.epfl.scala.debugadapter.internal.binary.*
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

  test("dotty stats"):
    val localClassCounter = new Counter[ClassType]()
    val innerClassCounter = new Counter[ClassType]()
    val anonClassCounter = new Counter[ClassType]()
    val topLevelClassCounter = new Counter[ClassType]()

    val localMethodCounter = new Counter[Method]()
    val anonFunCounter = new Counter[Method]()
    val localLazyInitCounter = new Counter[Method]()
    val methodCounter = new Counter[Method]()

    val jars = TestingResolver.fetch("org.scala-lang", "scala3-compiler_3", "3.3.1")
    val unpickler = new Scala3Unpickler(jars.map(_.absolutePath).toArray ++ javaRuntimeJars, println, testMode = true)

    for
      cls <- loadClasses(jars, "scala3-compiler_3-3.3.0")
      clsSym <- cls match
        case Patterns.LocalClass(_, _, _) => unpickler.process(cls, localClassCounter)
        case Patterns.AnonClass(_, _) => unpickler.process(cls, anonClassCounter)
        case Patterns.InnerClass(_) => unpickler.process(cls, innerClassCounter)
        case _ => unpickler.process(cls, topLevelClassCounter)
      method <- cls.declaredMethods
    do
      method match
        case Patterns.AnonFun(_) => unpickler.process(method, anonFunCounter)
        case Patterns.LocalLazyInit(_, _) => unpickler.process(method, localLazyInitCounter)
        case Patterns.LocalMethod(_, _) => unpickler.process(method, localMethodCounter)
        case _ => unpickler.process(method, methodCounter)
    localClassCounter.printStatus("local classes")
    anonClassCounter.printStatus("anon classes")
    innerClassCounter.printStatus("inner classes")
    topLevelClassCounter.printStatus("top level classes")
    localMethodCounter.printStatus("local methods")
    anonFunCounter.printStatus("anon fun")
    localLazyInitCounter.printStatus("local lazy inits")
    methodCounter.printStatus("other methods")
  // methodCounter.notFound.foreach{mthd => println(mthd.declaringClass.name) ; println(mthd.name)}

  def loadClasses(jars: Seq[Library], jarName: String): Seq[JavaReflectClass] =
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
            val reader = new asm.ClassReader(inputStream)
            val className = reader.getClassName.replace('/', '.')
            val lineNumbers = getLineNumbers(reader)
            JavaReflectClass(classLoader.loadClass(className), lineNumbers)
          }
          .toSeq
      }
      .get
    println(s"classNames: ${classes.size}")
    classes

  def getLineNumbers(reader: asm.ClassReader): Map[MethodSig, Seq[Int]] =
    var linesMap = Map.empty[MethodSig, Seq[Int]]
    val visitor =
      new asm.ClassVisitor(asm.Opcodes.ASM9):
        override def visitMethod(
            access: Int,
            name: String,
            descriptor: String,
            signature: String,
            exceptions: Array[String]
        ): asm.MethodVisitor =
          new asm.MethodVisitor(asm.Opcodes.ASM9):
            val lines = mutable.Set.empty[Int]
            override def visitLineNumber(line: Int, start: asm.Label): Unit =
              lines += line
            override def visitEnd(): Unit =
              val span = if lines.size > 1 then Seq(lines.min, lines.max) else lines.toSeq
              linesMap = linesMap + (MethodSig(name, descriptor) -> span)
    reader.accept(visitor, asm.Opcodes.ASM9)
    linesMap

  extension (unpickler: Scala3Unpickler)
    def process(cls: ClassType, counter: Counter[ClassType]): Option[Symbol] =
      try
        val sym = unpickler.findClass(cls)
        counter.addSuccess(cls)
        Some(sym.symbol)
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

    def process(mthd: Method, counter: Counter[Method]): Unit =
      try
        val sym = unpickler.findSymbol(mthd)
        counter.addSuccess(mthd)
      catch
        case NotFoundException(e) =>
          counter.addNotFound(mthd)
        case AmbiguousException(e) =>
          counter.addAmbiguous(mthd)
        case e =>
          counter.exceptions += e.toString

  override def munitTimeout: Duration = 2.minutes

  class Counter[T]:
    val success: mutable.Buffer[T] = mutable.Buffer.empty[T]
    var notFound: mutable.Buffer[T] = mutable.Buffer.empty[T]
    var ambiguous: mutable.Buffer[T] = mutable.Buffer.empty[T]
    var exceptions: mutable.Buffer[String] = mutable.Buffer.empty[String]

    def addSuccess(cls: T) = success += cls

    def addNotFound(cls: T) = notFound += cls

    def addAmbiguous(cls: T) = ambiguous += cls

    def printExceptions = exceptions.foreach(println(_))

    def printStatus(m: String) =
      println(s"$m:")
      println(s"  - total is ${ambiguous.size + notFound.size + success.size}")
      println(s"  - success is ${success.size}")
      println(s"  - ambiguous is ${ambiguous.size}")
      println(s"  - notFound is ${notFound.size}")
      println(s"  - exceptions is ${exceptions.size}")
