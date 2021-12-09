import lib.*

import scala.collection.mutable

object Day09 extends AocDay[Array[Array[Int]], Int]("data/day09") :
  val adj: List[(Int, Int)] = List((1, 0), (-1, 0), (0, 1), (0, -1));

  def parse(input: String): Array[Array[Int]] =
    input.split('\n').toList.map(line =>
      line.map(c => c - '0').toArray
    ).toArray

  def solve1(grid: Array[Array[Int]]): Int =
    lowPoints(grid).map((r, c) => grid(r)(c) + 1).sum

  def solve2(grid: Array[Array[Int]]): Int =
    val sources: List[(Int, Int)] = lowPoints(grid)
    val sizes = sources.map(bfs(grid)).sortWith(_ > _).toArray
    sizes(0) * sizes(1) * sizes(2)

  def lowPoints(grid: Array[Array[Int]]): List[(Int, Int)] =
    val points = range(0, grid.length - 1).flatMap(r =>
      range(0, grid(0).length - 1).map(c =>
        (r, c)
      ))
    points.filter((r, c) =>
      var isLow = true;
      if c > 0 then
        isLow &&= grid(r)(c) < grid(r)(c - 1)
      if c < grid(0).length - 1 then
        isLow &&= grid(r)(c) < grid(r)(c + 1);
      if r > 0 then
        isLow &&= grid(r)(c) < grid(r - 1)(c);
      if r < grid.length - 1 then
        isLow &&= grid(r)(c) < grid(r + 1)(c);
      isLow
    )

  def bfs(grid: Array[Array[Int]])(s: (Int, Int)): Int =
    val visited = Array.ofDim[Boolean](grid.length, grid(0).length)
    val q: mutable.Queue[(Int, Int)] = mutable.Queue();
    q.addOne(s)
    var size = 1

    def visit(r: Int, c: Int): Unit =
      if grid(r)(c) < 9 then
        visited(r)(c) = true
        q.addOne(r, c)
        size += 1

    while (q.nonEmpty) {
      val (r, c) = q.removeHead()
      if c > 0 && !visited(r)(c - 1) && grid(r)(c) < grid(r)(c - 1) then
        visit(r, c - 1)
      if c < grid(0).length - 1 && !visited(r)(c + 1) && grid(r)(c) < grid(r)(c + 1) then
        visit(r, c + 1)
      if r > 0 && !visited(r - 1)(c) && grid(r)(c) < grid(r - 1)(c) then
        visit(r - 1, c)
      if r < grid.length - 1 && !visited(r + 1)(c) && grid(r)(c) < grid(r + 1)(c) then
        visit(r + 1, c)
    }
    size
