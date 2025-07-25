package years.y2021

import lib.day.AocDay

object Day25 extends AocDay[Array[Array[Char]], Long]:
    def parse(text: String): Array[Array[Char]] =
        text.split('\n').map(line => line.toCharArray)

    def solve1(input: Array[Array[Char]]): Long =
        var grid = input
        val numRows = grid.length
        val numColumns = grid.head.length
        var newGrid = Array.ofDim[Char](grid.length, grid.head.length)

        def transform(): Boolean =
            for r <- grid.indices
                c <- grid.head.indices
            do
                newGrid(r)(c) = '.'

            var changed = false
            for
                r <- grid.indices
                c <- grid.head.indices
            do
                val nr = (r + 1) % numRows
                val nc = (c + 1) % numColumns

                if grid(r)(c) == '>' then
                    if grid(r)(nc) == '.' then
                        changed = true
                        newGrid(r)(nc) = '>'
                    else
                        newGrid(r)(c) = '>'

            for
                r <- grid.indices
                c <- grid.head.indices
            do
                val nr = (r + 1) % numRows
                val nc = (c + 1) % numColumns
                if grid(r)(c) == 'v' then
                    if grid(nr)(c) != 'v' && newGrid(nr)(c) == '.' then
                        changed = true
                        newGrid(nr)(c) = 'v'
                    else
                        newGrid(r)(c) = 'v'
            val tmp = grid
            grid = newGrid
            newGrid = tmp

            changed

        // Last step returns false and breaks out of the loop below
        // Therefore, pre-emptively set at 1
        var steps = 1
        while transform() do
            steps += 1
        steps

    def solve2(grid: Array[Array[Char]]): Long =
        58
