package y2021

import lib.*

import scala.collection.mutable

object Day09 extends AocDay[Array[Array[Int]], Int]("data/day09"):
  val adj: List[(Int, Int)] = List((1, 0), (-1, 0), (0, 1), (0, -1))

  def parse(input: String): Array[Array[Int]] =
    input.split('\n').toList.map(line => line.map(c => c - '0').toArray).toArray

  def solve1(grid: Array[Array[Int]]): Int =
    lowPoints(grid).map((r, c) => grid(r)(c) + 1).sum

  def solve2(grid: Array[Array[Int]]): Int =
    val sources: List[(Int, Int)] = lowPoints(grid)
    sources
      .map(basinSize(grid))
      .sortWith(_ > _)
      .take(3)
      .product

  def lowPoints(grid: Array[Array[Int]]): List[(Int, Int)] =
    val points = range(0, grid.length - 1).flatMap(r =>
      range(0, grid(0).length - 1).map(c => (r, c))
    )
    points.filter((r, c) =>
      adj.forall((dr, dc) =>
        if inside(grid)((r + dr, c + dc)) then grid(r)(c) < grid(r + dr)(c + dc)
        else true
      )
    )

  def inside(grid: Array[Array[Int]])(u: (Int, Int)): Boolean =
    val (r, c) = u
    0 <= r && r < grid.length && 0 <= c && c < grid(0).length

  def basinSize(grid: Array[Array[Int]])(s: (Int, Int)): Int =
    val visited = Array.ofDim[Boolean](grid.length, grid(0).length)
    val q: mutable.Queue[(Int, Int)] = mutable.Queue()
    q.addOne(s)
    visited(s._1)(s._2) = true
    var size = 1
    while (q.nonEmpty) {
      val (r, c) = q.removeHead()
      adj
        .map((dr, dc) => (r + dr, c + dc))
        .filter(inside(grid))
        .filter((ar, ac) => !visited(ar)(ac) && grid(ar)(ac) < 9)
        .foreach((ar, ac) =>
          q.addOne(ar, ac)
          visited(ar)(ac) = true
          size += 1
        )
    }
    size
