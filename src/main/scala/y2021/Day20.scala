package y2021

import lib.*

object Day20 extends AocDay[(Array[Int], Array[Array[Int]]), Int]("data/day20"):
  val adj: List[(Int, Int)] = product(range(-1, 1), range(-1, 1))

  def parse(input: String): (Array[Int], Array[Array[Int]]) =
    def strToIntArr(s: String): Array[Int] =
      s.map(c => if c == '#' then 1 else 0).toArray

    val parts = input.split("\n\n")
    val mask: Array[Int] = strToIntArr(parts(0))
    val img: Array[Array[Int]] = parts(1).split("\n").map(strToIntArr)
    (mask, img)

  def solve(iterations: Int)(data: (Array[Int], Array[Array[Int]])): Int =
    val mask = data._1
    var img = data._2
    var bg = 0

    for (_ <- 0 until iterations)
      val R = img.length
      val C = img(0).length
      val tImg = Array.fill(R + 4, C + 4)(bg)

      // Tricky part of this question
      if bg == 0 then
        if mask(0) == 1 then bg = 1
      else if mask((1 << 9) - 1) == 0 then bg = 0

      val newImg = Array.fill(R + 4, C + 4)(bg)

      for (i <- 0 until R)
        for (j <- 0 until C)
          tImg(i + 2)(j + 2) = img(i)(j)

      for (i <- 1 until R + 3)
        for (j <- 1 until C + 3)
          val bits = adj
            .map((dr, dc) => (i + dr, j + dc))
            .map((r, c) => tImg(r)(c))
            .mkString
          newImg(i)(j) = mask(Integer.parseInt(bits, 2))

      img = newImg

    img.map(row => row.sum).sum

  def solve1(data: (Array[Int], Array[Array[Int]])): Int = solve(2)(data)
  def solve2(data: (Array[Int], Array[Array[Int]])): Int = solve(50)(data)
