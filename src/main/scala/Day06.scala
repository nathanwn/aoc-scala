import lib.*

import scala.annotation.tailrec

object Day06 extends AocDay[List[Long], Long]("data/day06") :
  def parse(input: String): List[Long] =
    input.split(',').toList.map(s => s.toLong)

  def solve(finalDay: Int)(initTimers: List[Long]): Long =
    val counts = range(0, 8)
      .map(t => initTimers.count(_ == t)).toArray

    val D = finalDay;
    val T = 8;
    val f = Array.ofDim[Long](D + 1, T + 1)

    range(0, D).map(d =>
      range(0, T).map(t =>
        (d, t) match
          case (0, _) => f(d)(t) = counts(t)
          case (_, 6) => f(d)(t) = f(d - 1)(7) + f(d - 1)(0)
          case (_, 8) => f(d)(t) = f(d - 1)(0)
          case _ => f(d)(t) = f(d - 1)(t + 1)
      )
    )

    range(0, 8).map(t => f(finalDay)(t)).sum

  def solve1(data: List[Long]): Long = solve(80)(data)

  def solve2(data: List[Long]): Long = solve(256)(data)