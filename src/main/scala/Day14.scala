import lib.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day14 extends AocDay[(Array[Char], Map[String, Char]), Long]("data/day14"):
  def parse(input: String): (Array[Char], Map[String, Char]) =
    val parts = input.split("\n\n")
    val s = parts(0).toArray
    val rules = parts(1).split("\n").toList.map(line =>
      val tokens = line.split(" -> ")
      (tokens(0), tokens(1).charAt(0))
    ).toMap
    (s, rules)

  def solveSlow(steps: Int)(input: (Array[Char], Map[String, Char])): Int =
    var s = input._1
    val rules = input._2
    range(0, steps - 1).foreach(_ =>
      val insertions = range(0, s.length - 1).map(i =>
        if i < s.length - 1 then
          val curPair: String = s(i).toString + s(i + 1);
          rules.get(curPair)
        else None
      ).toArray
      val newLength = s.length + insertions.count(_.isDefined)
      val newS = Array.ofDim[Char](newLength)
      var k = 0;
      range(0, s.length - 1).foreach(i =>
        newS(k) = s(i)
        k += 1
        insertions(i) match {
          case Some(c) =>
            newS(k) = c
            k += 1
          case None =>
        }
      );
      s = newS
    )
    val countMap = s.groupBy(identity).view.mapValues(_.length)
    countMap.maxBy(_._2)._2 - countMap.minBy(_._2)._2

  def solveFast(steps: Int)(input: (Array[Char], Map[String, Char])): Long =
    val (s, rules) = input
    val pairs: List[String] = range(0, s.length - 2).map(i => s"${s(i)}${s(i + 1)}")
    val memoized: mutable.Map[(String, Int), mutable.Map[Char, Long]] = mutable.Map()

    def solvePair(pair: String, limit: Int): mutable.Map[Char, Long] =
      val c1 = pair.charAt(0)
      val c2 = pair.charAt(1)

      memoized.get((pair, limit)) match
        case Some(m) => m
        case None =>
          if limit == 0 then
            if c1 == c2 then mutable.Map(c1 -> 2) else mutable.Map(c1 -> 1, c2 -> 1)
          else
            rules.get(pair) match
              case None => if c1 == c2 then mutable.Map(c1 -> 2) else mutable.Map(c1 -> 1, c2 -> 1)
              case Some(c) =>
                val firstPair: String = s"$c1$c"
                val secondPair: String = s"$c$c2"
                val lhs = solvePair(firstPair, limit - 1)
                val rhs = solvePair(secondPair, limit - 1)
                val countMap = lhs ++ rhs.map{ case (k, v) => k -> (v + lhs.getOrElse(k, 0L)) }
                countMap(c) = countMap(c) - 1 // prevent double-counting the middle character
                memoized((pair, limit)) = countMap
                countMap

    val countMap = pairs.map(pair => solvePair(pair, steps))
      .foldLeft(mutable.Map[Char, Long]())
      ((lhs, rhs) => lhs ++ rhs.map{ case (k, v) => k -> (v + lhs.getOrElse(k, 0L)) })
    range(1, s.length - 2).foreach(i => countMap(s(i)) = countMap(s(i)) - 1)
    countMap.maxBy(_._2)._2 - countMap.minBy(_._2)._2

  def solve1(input: (Array[Char], Map[String, Char])): Long = solveSlow(10)(input)

  def solve2(input: (Array[Char], Map[String, Char])): Long = solveFast(40)(input)
