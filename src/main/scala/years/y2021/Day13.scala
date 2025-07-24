package years.y2021

import lib.day.AocDay

object Day13 extends AocDay[(List[(Int, Int)], List[(Int, Int)]), Int]:
    private final val VER = 0
    private final val HOR = 1

    def parse(input: String): (List[(Int, Int)], List[(Int, Int)]) =
        val parts = input.split("\n\n")
        val dots = parts(0)
            .split("\n")
            .toList
            .map(dot =>
                val coors = dot.split(",")
                (coors(0).toInt, coors(1).toInt)
            )
        val cmds = parts(1)
            .split("\n")
            .toList
            .map(line =>
                val cmdInput = line.split(" ")(2).split("=")
                if cmdInput(0) == "x" then (VER, cmdInput(1).toInt)
                else (HOR, cmdInput(1).toInt)
            )
        (dots, cmds)

    def solve1(input: (List[(Int, Int)], List[(Int, Int)])): Int =
        val (dots, cmds) = input
        val xMax = dots.map((x, _) => x).max
        val yMax = dots.map((_, y) => y).max
        var grid = Array.fill(xMax + 1, yMax + 1)(false)
        dots.foreach((x, y) => grid(x)(y) = true)
        grid = fold(cmds.head)(grid)
        grid.map(row => row.count(_ == true)).sum

    def solve2(input: (List[(Int, Int)], List[(Int, Int)])): Int =
        val (dots, cmds) = input
        val xMax = dots.map((x, _) => x).max
        val yMax = dots.map((_, y) => y).max
        var grid = Array.fill(xMax + 1, yMax + 1)(false)
        dots.foreach((x, y) => grid(x)(y) = true)
        cmds.foreach(cmd => grid = fold(cmd)(grid))
        grid(0).indices.foreach(y =>
            grid.indices.foreach(x =>
                if grid(x)(y) then print("#") else print(" ")
            )
            println();
        )
        grid.map(row => row.count(_ == true)).sum

    private def fold(
        cmd: (Int, Int)
    )(grid: Array[Array[Boolean]]): Array[Array[Boolean]] =
        if cmd(0) == HOR
        then foldHor(cmd(1))(grid)
        else foldVer(cmd(1))(grid)

    private def foldVer(X: Int)(grid: Array[Array[Boolean]]): Array[Array[Boolean]] =
        val Y = grid(0).length
        val newGrid = Array.ofDim[Boolean](X, Y)
        (for
            x <- 0 until X;
            y <- 0 until Y
        yield (x, y)).foreach((x, y) =>
            newGrid(x)(y) =
                if 2 * X - x >= grid.length
                then grid(x)(y)
                else grid(x)(y) || grid(2 * X - x)(y)
        )
        newGrid

    private def foldHor(Y: Int)(grid: Array[Array[Boolean]]): Array[Array[Boolean]] =
        val X = grid.length
        val newGrid = Array.ofDim[Boolean](X, Y)
        (for
            x <- 0 until X;
            y <- 0 until Y
        yield (x, y)).foreach((x, y) =>
            newGrid(x)(y) =
                if 2 * Y - y >= grid(0).length
                then grid(x)(y)
                else grid(x)(y) || grid(x)(2 * Y - y)
        )
        newGrid
