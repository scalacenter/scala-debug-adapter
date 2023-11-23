package ch.epfl.scala.debugadapter.internal.stacktrace

import java.io.PrintWriter
import java.nio.file.Paths
import scala.collection.mutable
import scala.io.Source
import scala.util.Properties
import scala.util.Random

class BinaryDecoderStatsFull extends BinaryDecoderStatsBase:
  lazy val file = Paths.get(s"test-result-${Random.nextInt(Int.MaxValue)}.txt")
  lazy val pw = new PrintWriter(file.toFile)

  override def println(x: Any): Unit =
    Predef.println(x)
    pw.println(x)
    pw.flush()

  test("all Scala 3 ecosystem".ignore):
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
      .sortBy(count => -count.successPercent)
      .foreach(c => println(s"${c.name} ${c.successPercent}%"))
