package years.y2021

import lib.day.AocDay

import scala.annotation.tailrec
import scala.collection.mutable

object Day11 extends AocDay[Array[Array[Int]], Int]:
    private final val SIZE = 10
    val adj: Seq[(Int, Int)] =
        for
            i <- -1 to 1;
            j <- -1 to 1
            if (i, j) != (0, 0)
        yield (i, j)

    def parse(input: String): Array[Array[Int]] =
        input
            .split('\n')
            .toList
            .map(line => line.map(c => c - '0').toArray)
            .toArray

    def solve1(grid: Array[Array[Int]]): Int =
        def stepTransform(step: Int): Int =
            if (step == 100) 0
            else
                for
                    r <- 0 until SIZE;
                    c <- 0 until SIZE
                do grid(r)(c) += 1

                autoTransform(grid) + stepTransform(step + 1)

        stepTransform(0)

    def solve2(grid: Array[Array[Int]]): Int =
        @tailrec
        def stepTransform(step: Int): Int =
            for
                r <- 0 until SIZE;
                c <- 0 until SIZE
            do grid(r)(c) += 1

            autoTransform(grid)
            if grid.forall(row => row.forall(_ == 0))
            then step + 1
            else stepTransform(step + 1)

        stepTransform(0)

    private def autoTransform(grid: Array[Array[Int]]): Int =
        val flashing: Seq[(Int, Int)] =
            for
                r <- 0 until SIZE;
                c <- 0 until SIZE
                if grid(r)(c) == 10
            yield (r, c)

        if flashing.isEmpty then return 0
        flashing.foreach((r, c) =>
            adj
                .map((dr, dc) => (r + dr, c + dc))
                .filter(inside(grid))
                .filter((nr, nc) => grid(nr)(nc) != 10 && grid(nr)(nc) != 0)
                .foreach((nr, nc) => grid(nr)(nc) += 1)
        )
        flashing.foreach((r, c) => grid(r)(c) = 0)
        flashing.size + autoTransform(grid)

    private def inside(grid: Array[Array[Int]])(u: (Int, Int)): Boolean =
        val (r, c) = u
        0 <= r && r < grid.length && 0 <= c && c < grid(0).length

    private def printGrid(grid: Array[Array[Int]]): Unit =
        grid.foreach(row =>
            row.foreach(cell => print(cell))
            println()
        )
