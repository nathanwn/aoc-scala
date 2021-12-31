import lib.*

import scala.collection.mutable

object Day21 extends AocDay[Array[Int], Long]("data/day21"):
  def parse(input: String): Array[Int] =
    input.split('\n')
      .map(line => Integer.parseInt(line.split(": ").toList.last))

  def solve1(starts: Array[Int]): Long =
    val scores = Array.fill[Int](2)(0)
    val pos = starts.map(_ - 1)
    var dice = 1
    var who = 0
    var rollCount = 0

    def roll(): Int =
      rollCount += 1
      var res = 0
      for (i <- 0 until 3)
        res += dice
        dice = dice % 100 + 1
      res

    while scores.max < 1000 do
      pos(who) = (pos(who) + roll()) % 10
      scores(who) += pos(who) + 1
      who ^= 1

    scores.min * rollCount * 3

  def solve2(starts: Array[Int]): Long =
    val dp: mutable.Map[(Int, Int, Int, Int), (Long, Long)] = mutable.Map()

    // X is rolling, Y is waiting
    def calcWins(sx: Int, sy: Int, px: Int, py: Int): (Long, Long) =
      if dp.contains((sx, sy, px, py)) then
        return dp((sx, sy, px, py))
      if sx >= 21 then return (1L, 0L)
      if sy >= 21 then return (0L, 1L)

      var ans = (0L, 0L)

      for (d1 <- 1 to 3)
        for (d2 <- 1 to 3)
          for (d3 <- 1 to 3)
            val newPx = (px + d1 + d2 + d3) % 10
            val newSx = sx + newPx + 1
            // Now it's Y's turn to roll
            val (yWins, xWins) = calcWins(sy, newSx, py, newPx)
            ans = (ans._1 + xWins, ans._2 + yWins)

      dp((sx, sy, px, py)) = ans
      ans

    val wins = calcWins(0, 0, starts(0) - 1, starts(1) - 1)
    if wins._1 > wins._2 then wins._1 else wins._2