package ch.epfl.scala.debugadapter.internal.stacktrace

import java.io.PrintWriter
import java.nio.file.Paths
import scala.collection.mutable
import scala.io.Source
import scala.util.Properties
import scala.util.Random
import coursier.maven.MavenRepository
import ch.epfl.scala.debugadapter.testfmk.FetchOptions
import ch.epfl.scala.debugadapter.testfmk.TestingResolver
import coursier.error.ResolutionError
import ch.epfl.scala.debugadapter.Library

class BinaryDecoderStatsFull extends BinaryDecoderSuite:
  lazy val file = Paths.get(s"test-result-${Random.nextInt(Int.MaxValue)}.txt")
  lazy val pw = new PrintWriter(file.toFile)

  override protected def defaultThrowOrWarn: ThrowOrWarn = ThrowOrWarn.ignore
  override def println(x: Any): Unit =
    Predef.println(x)
    pw.println(x)
    pw.flush()

  test("all Scala 3 ecosystem".ignore)(decodeAllFromCsvFile("scala3-artifacts-231121.csv"))
  test("all failed".ignore)(decodeAllFromCsvFile("scala3-artifacts-231121-failed.csv"))

  def decodeAllFromCsvFile(fileName: String)(using ThrowOrWarn): Unit =
    val csv = Source.fromResource(fileName)
    val classCounts = mutable.Buffer.empty[Count]
    val methodCounts = mutable.Buffer.empty[Count]
    for line <- csv.getLines.drop(1) do
      val parts = line.split(',').map(_.drop(1).dropRight(1))
      val (org, artifact, version) = (parts(0), parts(1), parts(2))
      try
        val (classCounter, methodCounter) = tryDecodeAll(org, artifact, version)
        classCounts += classCounter.count
        methodCounts += methodCounter.count
      catch case e => println(s"cannot decode $line")

    val totalClassCounter = classCounts.foldLeft(Count("total classes"))(_.merge(_))
    val totalMethodCounter = methodCounts.foldLeft(Count("total methods"))(_.merge(_))
    totalClassCounter.printReport()
    totalMethodCounter.printReport()

    (classCounts ++ methodCounts).toSeq
      .sortBy(count => -count.successPercent)
      .foreach(c => println(s"${c.name} ${c.successPercent}%"))

  def tryDecodeAll(org: String, artifact: String, version: String)(using ThrowOrWarn): (Counter, Counter) =
    val repositories =
      if org == "org.clulab" then Seq(MavenRepository("http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release"))
      else if org == "com.zengularity" then
        Seq(MavenRepository("https://oss.sonatype.org/content/repositories/snapshots"))
      else if org == "com.evolution" then Seq(MavenRepository("https://evolution.jfrog.io/artifactory/public"))
      else if org == "com.github.j5ik2o" then Seq(MavenRepository("https://maven.seasar.org/maven2/"))
      else Seq.empty
    def tryWith(keepOptional: Boolean, keepProvided: Boolean): Option[(Counter, Counter)] =
      try
        val fetchOptions = FetchOptions(keepOptional, keepProvided, repositories)
        val decoder = initDecoder(org, artifact, version, fetchOptions)
        Some(decoder.decodeAll())
      catch case _ => None
    tryWith(false, false)
      .orElse(tryWith(false, true))
      .orElse(tryWith(true, false))
      .orElse(tryWith(true, true))
      .get
