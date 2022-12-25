package years.y2021

import scala.annotation.tailrec
import lib.day.AocDay

object Day06 extends AocDay[List[Long], Long]("data/y2021/Day06"):
  def parse(input: String): List[Long] =
    input.split(',').toList.map(s => s.toLong)

  def solve(finalDay: Int)(initTimers: List[Long]): Long =
    val counts = (0 to 8)
      .map(t => initTimers.count(_ == t))
      .toArray

    val D = finalDay;
    val T = 8;
    val f = Array.ofDim[Long](D + 1, T + 1)

    (0 to D).map(d =>
      (0 to T).map(t =>
        (d, t) match
          case (0, _) => f(d)(t) = counts(t)
          case (_, 6) => f(d)(t) = f(d - 1)(7) + f(d - 1)(0)
          case (_, 8) => f(d)(t) = f(d - 1)(0)
          case _      => f(d)(t) = f(d - 1)(t + 1)
      )
    )

    (0 to 8).map(t => f(finalDay)(t)).sum

  def solve1(data: List[Long]): Long = solve(80)(data)

  def solve2(data: List[Long]): Long = solve(256)(data)
