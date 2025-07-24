package years.y2021

import lib.day.AocDay

object Day01 extends AocDay[List[Int], Int]:
    def parse(s: String): List[Int] =
        s.split('\n').map(_.toInt).toList

    def solve(skip: Int)(data: List[Int]): Int =
        val nexts: List[Int] = data.drop(skip)
        val pairs: List[(Int, Int)] = data.zip(nexts)
        pairs.map((cur, next) => cur < next).count(_ == true)

    def solve1(data: List[Int]): Int =
        solve(1)(data)

    def solve2(data: List[Int]): Int =
        solve(3)(data)
