package years.y2021

import lib.day.AocDay

import scala.collection.mutable

object Day10 extends AocDay[List[List[Char]], Long]("data/y2021/Day10"):
  val leftBrackets: List[Char] = List[Char]('(', '[', '{', '<')
  val rightBrackets: List[Char] = List[Char](')', ']', '}', '>')
  val cScores: List[Int] = List[Int](3, 57, 1197, 25137)
  val iScore: Seq[Int] = 1 to 4
  val rightOf: Map[Char, Char] =
    leftBrackets.zip(rightBrackets).map((l, r) => l -> r).toMap
  val leftOf: Map[Char, Char] =
    rightBrackets.zip(leftBrackets).map((r, l) => r -> l).toMap
  val cScoreOf: Map[Char, Int] =
    rightBrackets.zip(cScores).map((r, s) => r -> s).toMap
  val iScoreOf: Map[Char, Int] =
    leftBrackets.zip(iScore).map((r, s) => r -> s).toMap

  def parse(input: String): List[List[Char]] =
    input.split('\n').toList.map(line => line.toList)

  def solve1(lines: List[List[Char]]): Long =
    def solveLine(line: List[Char]): Long =
      val stack = mutable.Stack[Char]()
      line.foreach(c =>
        if leftBrackets.contains(c) then stack.push(c)
        else if rightBrackets.contains(c) then
          if c == rightOf(stack.top) then stack.pop
          else return cScoreOf(c)
      )
      0

    lines.map(solveLine).sum

  def solve2(lines: List[List[Char]]): Long =
    def solveLine(line: List[Char]): Long =
      val stack = mutable.Stack[Char]()
      line.foreach(c =>
        if leftBrackets.contains(c) then stack.push(c)
        else if rightBrackets.contains(c) then
          if c == rightOf(stack.top) then stack.pop
          else return 0
      )
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
