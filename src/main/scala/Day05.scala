import Day05.Segment
import lib.{AocDay, isTrue, range}

object Day05 extends AocDay[List[Segment], Int]("data/day05") :
  class Point2D(val x: Int, val y: Int)

  class Segment(val p1: Point2D, val p2: Point2D):
    def points(countDiagonals: Boolean = false): List[Point2D] = {
      if (p1.x == p2.x)
        val minY = math.min(p1.y, p2.y)
        val maxY = math.max(p1.y, p2.y)
        range(minY, maxY).map(y => Day05.Point2D(p1.x, y))
      else if (p1.y == p2.y)
        val minX = math.min(p1.x, p2.x)
        val maxX = math.max(p1.x, p2.x)
        range(minX, maxX).map(x => Day05.Point2D(x, p1.y))
      else if (countDiagonals)
        val xStep = if p1.x < p2.x then 1 else -1
        val yStep = if p1.y < p2.y then 1 else -1
        val xs = range(p1.x, p2.x, xStep)
        val ys = range(p1.y, p2.y, yStep)
        xs.zip(ys).map((x, y) => Day05.Point2D(x, y))
      else
        List[Point2D]()
    }

  def parsePoint(input: String): Point2D =
    val coors = input.split(',')
    Point2D(coors(0).toInt, coors(1).toInt)

  def parse(input: String): List[Segment] =
    input.split('\n').toList.map(line =>
      val points = line.split(" -> ")
        Segment(parsePoint(points(0)), parsePoint(points(1)))
    )

  def solve(countDiagonals: Boolean)(segments: List[Segment]): Int =
    val points = segments.flatMap(segment => segment.points(countDiagonals))
    val pointCounts = points.groupBy(p => (p.x, p.y)).view.mapValues(_.size)
    pointCounts.toList.filter((_, count) => count > 1).length

  def solve1(data: List[Segment]): Int = solve(countDiagonals = false)(data)

  def solve2(data: List[Segment]): Int = solve(countDiagonals = true)(data)