package years.y2021

import lib.day.AocDay

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

object Day10 extends AocDay[List[List[Char]], Long]:
  private val leftBrackets: List[Char] = List[Char]('(', '[', '{', '<')
  private val rightBrackets: List[Char] = List[Char](')', ']', '}', '>')
  private val cScores: List[Int] = List[Int](3, 57, 1197, 25137)
  private val iScore: Seq[Int] = 1 to 4
  private val rightOf: Map[Char, Char] =
    leftBrackets.zip(rightBrackets).map((l, r) => l -> r).toMap
  private val leftOf: Map[Char, Char] =
    rightBrackets.zip(leftBrackets).map((r, l) => r -> l).toMap
  private val cScoreOf: Map[Char, Int] =
    rightBrackets.zip(cScores).map((r, s) => r -> s).toMap
  private val iScoreOf: Map[Char, Int] =
    leftBrackets.zip(iScore).map((r, s) => r -> s).toMap

  def parse(input: String): List[List[Char]] =
    input.split('\n').toList.map(line => line.toList)

  def solve1(lines: List[List[Char]]): Long =
    def solveLine(line: List[Char]): Long =
      val stack = mutable.Stack[Char]()
      var res = 0
      breakable {
        line.foreach(c =>
          if leftBrackets.contains(c) then stack.push(c)
          else if rightBrackets.contains(c) then
            if c == rightOf(stack.top) then stack.pop
            else
              res = cScoreOf(c)
              break
        )
      }
      res

    lines.map(solveLine).sum

  def solve2(lines: List[List[Char]]): Long =
    def solveLine(line: List[Char]): Long =
      val stack = mutable.Stack[Char]()
      var res = -1
      breakable {
        line.foreach(c =>
          if leftBrackets.contains(c) then stack.push(c)
          else if rightBrackets.contains(c) then
            if c == rightOf(stack.top) then stack.pop
            else
              res = 0
              break
        )
      }
      if res == 0 then 0
      else
        stack.toList
          .map(c => iScoreOf(c))
          .map(_.toLong)
          .foldLeft(0L)((lhs, rhs) => lhs * 5 + rhs)

    val scores = lines
      .map(solveLine)
      .filter(score => score != 0)
      .sorted
      .toArray
    scores(scores.length / 2)
