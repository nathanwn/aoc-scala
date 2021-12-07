package lib


import scala.io.Source
import scala.util.{Try, Using}

abstract class AocDay[DType, RType <: AnyVal](val dataDir: String) {
  def parse(text: String): DType

  def solve1(data: DType): RType

  def solve2(data: DType): RType

  def getText(path: String): String =
    Using(Source.fromFile(path)) {
      source => source.mkString
    }.get

  def run(): Unit =
    val parseInputFile = (file: String) => parse(getText(file))
    val sampleData = parseInputFile(s"${dataDir}/sample.txt")
    val data = parseInputFile(s"${dataDir}/input.txt")
    println("Task 1")
    println(s"Sample: ${solve1(sampleData)}")
    println(s"Answer: ${solve1(data)}")
    println("Task 2")
    println(s"Sample: ${solve2(sampleData)}")
    println(s"Answer: ${solve2(data)}")
}