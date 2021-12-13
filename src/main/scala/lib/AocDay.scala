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
    def runSolver(solver: DType => RType, taskId: Int): Unit =
      val parseInputFile = (file: String) => parse(getText(file))
      val sampleData = parseInputFile(s"$dataDir/sample.txt")
      val data = parseInputFile(s"$dataDir/input.txt")
      println(s"Task $taskId")
      println(s"Sample: ${solver(sampleData)}")
      println(s"Answer: ${solver(data)}")
    runSolver(solve1, 1)
    runSolver(solve2, 2)
}