import Day22.Cmd
import lib.*

import scala.collection.mutable

object Day22 extends AocDay[List[Cmd], Long]("data/day22"):
  class Cmd(val signal: Int,
            val xMin: Int, val xMax: Int,
            val yMin: Int, val yMax: Int,
            val zMin: Int, val zMax: Int)

  def parse(input: String): List[Cmd] =
    val res: List[Cmd] = input.split('\n').toList.map(line =>
      val parts = line.split(' ')
      val signal: Int = if parts(0) == "on" then 1 else 0
      val c: Array[Int] = parts(1).split(',')
        .flatMap(s =>
          s.substring(2).split("\\.\\.")
        ).map(Integer.parseInt)
      Cmd(signal, c(0), c(1), c(2), c(3), c(4), c(5))
    )
    res

  def solve1(commands: List[Cmd]): Long =
    val m: mutable.Map[(Int, Int, Int), Int] = mutable.Map()
    for (x <- -50 to 50)
      for (y <- -50 to 50)
        for (z <- -50 to 50)
          m((x, y, z)) = 0
    commands.foreach(c =>
      for (x <- Math.max(c.xMin, -50) to Math.min(c.xMax, 50))
        for (y <- Math.max(c.yMin, -50) to Math.min(c.yMax, 50))
          for (z <- Math.max(c.zMin, -50) to Math.min(c.zMax, 50))
            m((x, y, z)) = c.signal
    )
    m.values.count(_ == 1)

  // Coordinate compression solution
  def solve2(commands: List[Cmd]): Long =
    val xs = commands.flatMap(c => List(c.xMin, c.xMax + 1)).sorted.toArray
    val ys = commands.flatMap(c => List(c.yMin, c.yMax + 1)).sorted.toArray
    val zs = commands.flatMap(c => List(c.zMin, c.zMax + 1)).sorted.toArray
    val grid = Array.fill[Int](xs.length, ys.length, zs.length)(0)

    // Find the last element x* in xs s/t x* <= x
    def getIndex(xs: Array[Int], x: Int): Int =
      var low = 0
      var high = xs.length - 1
      var ans = - 1
      while low <= high do
        val mid = low + (high - low) / 2
        if xs(mid) <= x then
          ans = mid
          low = mid + 1
        else
          high = mid - 1
      ans

    commands.foreach(c =>
      range(getIndex(xs, c.xMin), getIndex(xs, c.xMax)).foreach(xi =>
        range(getIndex(ys, c.yMin), getIndex(ys, c.yMax)).foreach(yi =>
          range(getIndex(zs, c.zMin), getIndex(zs, c.zMax)).foreach(zi =>
            grid(xi)(yi)(zi) = c.signal
          )
        )
      )
    )
    var res = 0L
    range(0, xs.length - 2).foreach(xi =>
      range(0, ys.length - 2).foreach(yi =>
        range(0, zs.length - 2).foreach(zi =>
          res = res + 1L * grid(xi)(yi)(zi)
              * (xs(xi + 1) - xs(xi))
              * (ys(yi + 1) - ys(yi))
              * (zs(zi + 1) - zs(zi))
        )
      )
    )
    res