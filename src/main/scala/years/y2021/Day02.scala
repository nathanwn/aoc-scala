package years.y2021

import lib.day.AocDay

import scala.collection.mutable

object Day02 extends AocDay[Seq[(String, Int)], Int]:
  def parse(input: String): Seq[(String, Int)] =
    input
      .split("\n")
      .map(line =>
        val lineTokens = line.split(" ")
        (lineTokens(0), lineTokens(1).toInt)
      )
      .toList

  def solve1(commands: Seq[(String, Int)]): Int =
    val x = commands
      .filter((d, _) => d == "forward")
      .map((_, k) => k)
      .sum
    val y = commands
      .filter((d, _) => d == "up" || d == "down")
      .map((d, k) =>
        d match
          case "up"   => -k
          case "down" => k
      )
      .sum
    x * y

  def solve2(commands: Seq[(String, Int)]): Int =
    var x = 0
    var y = 0
    var aim = 0
    for (d, k) <- commands
    do
      d match
        case "down" =>
          aim += k
        case "up" =>
          aim -= k
        case "forward" =>
          x += k
          y += aim * k
    x * y
