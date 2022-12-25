package years.y2021

import lib.day.AocDay

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day17 extends AocDay[((Int, Int), (Int, Int)), Int]("data/y2021/Day17"):
  def parse(input: String): ((Int, Int), (Int, Int)) =
    val coors = input.split(": ")(1).split(", ")
    (parseCoors(coors(0)), parseCoors(coors(1)))

  def parseCoors(s: String): (Int, Int) =
    val coors: Array[String] = s.split("=")(1).split("\\.\\.")
    (coors(0).toInt, coors(1).toInt)

  def travel(xRange: (Int, Int), yRange: (Int, Int))(
      vx0: Int,
      vy0: Int
  ): Option[List[(Int, Int)]] =
    val (xMin, xMax) = xRange
    val (yMin, yMax) = yRange
    var (vx, vy) = (vx0, vy0)
    var (x, y) = (0, 0)
    val coors = new ListBuffer[(Int, Int)]
    coors.append((x, y))
    while !(vy < 0 && y < yMin) do
      x += vx
      y += vy
      coors.append((x, y))
      vx = if vx > 0 then vx - 1 else if vx < 0 then vx + 1 else 0
      vy -= 1
    if coors.nonEmpty then Some(coors.toList) else None

  def solve1(input: ((Int, Int), (Int, Int))): Int =
    val (xMin, xMax) = input._1
    val (yMin, yMax) = input._2

    (for
      vx <- -200 to 200;
      vy <- -200 to 200
    yield (vx, vy))
      .map((vx, vy) =>
        travel((xMin, xMax), (yMin, yMax))(vx, vy) match {
          case None => Integer.MIN_VALUE
          case Some(coors) =>
            val within = coors
              .filter((x, y) =>
                x >= xMin && x <= xMax && y >= yMin && y <= yMax
              )
            if within.isEmpty then Integer.MIN_VALUE
            else coors.maxBy((x, y) => y)._2
        }
      )
      .max

  def solve2(input: ((Int, Int), (Int, Int))): Int =
    val (xMin, xMax) = input._1
    val (yMin, yMax) = input._2

    (for
      vx <- -200 to 200;
      vy <- -200 to 200
    yield (vx, vy))
      .map((vx, vy) =>
        travel((xMin, xMax), (yMin, yMax))(vx, vy) match {
          case None => 0
          case Some(coors) =>
            val within = coors
              .filter((x, y) =>
                x >= xMin && x <= xMax && y >= yMin && y <= yMax
              )
            if within.isEmpty then 0
            else 1
        }
      )
      .sum
