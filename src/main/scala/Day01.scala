import lib.*

type Data = List[Int]

def parseInput(s: String): Data =
  s.split('\n').map(_.toInt).toList

def solve(skip: Int)(data: Data): Int =
  val nexts: List[Int] = data.drop(skip)
  val pairs: List[(Int, Int)] = data.zip(nexts)
  pairs.map((cur, next) => cur < next).count(isTrue)

def solve1(data: Data): Int =
  solve(1)(data)

def solve2(data: Data): Int =
  solve(3)(data)

@main def day01: Unit =
  val parse = (file: String) => parseInput(scala.io.Source.fromFile(file).mkString)
  val sampleData: Data = parse("data/day01/sample.txt")
  val data: Data = parse("data/day01/input.txt")
  println("Task 1")
  println(s"Sample: ${solve1(sampleData)}")
  println(s"Answer: ${solve1(data)}")
  println("Task 2")
  println(s"Sample: ${solve2(sampleData)}")
  println(s"Answer: ${solve2(data)}")