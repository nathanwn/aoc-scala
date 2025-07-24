package years.y2021

import Day05.Segment
import lib.day.AocDay
import lib.geometry

object Day05 extends AocDay[List[Segment], Int]:
    class Point(override val x: Int, override val y: Int)
        extends geometry.Point[Int](x, y)

    class Segment(val p1: Point, val p2: Point)
        extends geometry.Segment[Int](p1, p2):
        def interpolate(doCountDiagonals: Boolean = false): Seq[Point] =
            if p1.x == p2.x then
                val minY = math.min(p1.y, p2.y)
                val maxY = math.max(p1.y, p2.y)
                (minY to maxY)
                    .map(y => Point(p1.x, y))
            else if p1.y == p2.y then
                val minX = math.min(p1.x, p2.x)
                val maxX = math.max(p1.x, p2.x)
                (minX to maxX)
                    .map(x => Point(x, p1.y))
            else if doCountDiagonals then
                val xStep = if p1.x < p2.x then 1 else -1
                val yStep = if p1.y < p2.y then 1 else -1
                (p1.x to p2.x by xStep)
                    .zip(p1.y to p2.y by yStep)
                    .map((x, y) => Point(x, y))
            else List[Point]()

    private def parsePoint(input: String): Point =
        val coors = input.split(',')
        Point(coors(0).toInt, coors(1).toInt)

    def parse(input: String): List[Segment] =
        input
            .split('\n')
            .toList
            .map(line =>
                val points = line.split(" -> ")
                Segment(parsePoint(points(0)), parsePoint(points(1)))
            )

    def solve(doCountDiagonals: Boolean)(segments: List[Segment]): Int =
        val points: Seq[Point] = segments
            .flatMap(segment => segment.interpolate(doCountDiagonals))
        val pointCounts = points.groupBy(p => (p.x, p.y)).view.mapValues(_.size)
        pointCounts.toList.count((_, count) => count > 1)

    def solve1(data: List[Segment]): Int = solve(doCountDiagonals = false)(data)

    def solve2(data: List[Segment]): Int = solve(doCountDiagonals = true)(data)
