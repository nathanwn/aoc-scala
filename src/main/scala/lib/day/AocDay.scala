package lib.day

import scala.io.Source
import scala.util.{Try, Using}

abstract class AocDay[InputType, OutputType <: AnyVal](val dataDir: String) {
  def parse(text: String): InputType

  def solve1(data: InputType): OutputType

  def solve2(data: InputType): OutputType

  def run(taskId: Int): Unit =
    println(s"Task $taskId")
    println(s"Sample: ${solve(taskId, "sample.txt")}")
    println(s"Answer: ${solve(taskId, "input.txt")}")

  def solve(taskId: Int, fileName: String): OutputType =
    val data = parseInputFile(fileName)
    val solver: InputType => OutputType = getSolver(taskId)
    solver(data)

  def parseInputFile(fileName: String): InputType =
    val fileContent = Using(Source.fromFile(s"$dataDir/$fileName")) { source =>
      source.mkString
    }.get
    parse(fileContent)

  def getSolver(taskId: Int): InputType => OutputType =
    taskId match
      case 1 => solve1
      case 2 => solve2
}
