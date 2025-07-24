package years.y2021

import lib.day.AocDay

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day12 extends AocDay[Map[String, List[String]], Int]:
  def parse(input: String): Map[String, List[String]] =
    var g: Map[String, ListBuffer[String]] = Map()
    input
      .split('\n')
      .toList
      .map(line =>
        val e = line.split('-')
        (e(0), e(1))
      )
      .foreach((u, v) =>
        if !g.contains(u) then g = g + (u -> ListBuffer())
        if !g.contains(v) then g = g + (v -> ListBuffer())
        g(u) += v
        g(v) += u
      )
    g.map((u, vs) => u -> vs.toList).toMap

  def solve1(g: Map[String, List[String]]): Int =
    val visits = mutable.Map[String, Int]()
    g.foreach((u, _) => visits += (u -> 0))
    var pathCounts = 0

    def traverse(u: String): Unit =
      if u == "end" then pathCounts += 1
      else
        visits(u) += 1
        g(u)
          .filter(v => v != "start")
          .filter(v => !(Character.isLowerCase(v.charAt(0)) && visits(v) == 1))
          .foreach(traverse)
        visits(u) -= 1

    traverse("start")
    pathCounts

  def solve2(g: Map[String, List[String]]): Int =
    val visits = mutable.Map[String, Int]()
    g.foreach((u, _) => visits += (u -> 0))
    var pathCounts = 0

    def traverse(u: String, smallTwiceAlready: Boolean): Unit =
      if u == "end" then pathCounts += 1
      else
        visits(u) += 1
        val smallLimit = if smallTwiceAlready then 1 else 2
        g(u)
          .filter(v => v != "start")
          .filter(v =>
            !(Character.isLowerCase(v.charAt(0)) && visits(v) >= smallLimit)
          )
          .foreach(v =>
            if Character.isLowerCase(v.charAt(0)) then
              if visits(v) == 0 then traverse(v, smallTwiceAlready)
              else if !smallTwiceAlready then traverse(v, true)
            else traverse(v, smallTwiceAlready)
          )
        visits(u) -= 1

    traverse("start", false)
    pathCounts
