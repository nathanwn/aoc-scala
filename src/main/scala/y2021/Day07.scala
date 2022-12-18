package y2021

import lib.{AocDay, isTrue, range}

// Page 61 of CP Handbook mentions this problem
object Day07 extends AocDay[List[Int], Int]("data/day07"):
  def parse(input: String): List[Int] =
    input.split(',').toList.map(s => s.toInt)

  def solve1(xs: List[Int]): Int =
    val xsSorted = xs.sorted.toArray
    val median = xsSorted(xs.length / 2)
    xs.map(x => Math.abs(median - x)).sum

  def solve2(xs: List[Int]): Int =
    val xsAverage: Int = xs.sum / xs.length
    range(xsAverage, xsAverage + 1)
      .map(avg =>
        xs.map(x =>
          val d = Math.abs(avg - x)
          d * (d + 1) / 2
        ).sum
          .toInt
      )
      .min
