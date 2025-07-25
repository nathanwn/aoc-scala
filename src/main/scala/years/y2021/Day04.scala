package years.y2021

import lib.day.AocDay

import scala.collection.mutable.ListBuffer

object Day04 extends AocDay[(List[Int], Array[Array[Array[Int]]]), Long]:
    def parse(text: String): (List[Int], Array[Array[Array[Int]]]) =
        val parts = text.split("\n\n")
        val nums = parts(0).split(",").map(_.toInt).toList
        val boards: Array[Array[Array[Int]]] = (1 until parts.length)
            .map(partId =>
                val boardInput = parts(partId)
                parseBoard(boardInput)
            )
            .toArray
        (nums, boards)

    private def parseBoard(boardInput: String): Array[Array[Int]] =
        boardInput
            .split("\n")
            .map(line => line.strip().split("\\s+").map(_.toInt))

    def solve1(data: (List[Int], Array[Array[Array[Int]]])): Long =
        val (nums, boards) = data
        solveWinningBoardSequence(nums, boards).head._2

    def solve2(data: (List[Int], Array[Array[Array[Int]]])): Long =
        val (nums, boards) = data
        solveWinningBoardSequence(nums, boards).last._2

    private def solveWinningBoardSequence(
        nums: List[Int],
        boards: Array[Array[Array[Int]]]
    ): List[(Int, Long)] =
        val mark = Array.ofDim[Boolean](
          boards.length,
          boards.head.length,
          boards.head.head.length
        )
        val res: ListBuffer[(Int, Long)] = ListBuffer()
        nums.foreach(num =>
            for
                i <- boards.indices
                r <- boards.head.indices
                c <- boards.head.head.indices
            do if boards(i)(r)(c) == num then mark(i)(r)(c) = true
            boards.indices
                .filter(i => res.forall((id, _) => id != i))
                .foreach(i =>
                    val winningScore: Option[Long] =
                        checkWinningScore(boards(i), mark(i), num)
                    winningScore match
                        case Some(value) => {
                            res.append((i, value))
                        }
                        case None =>
                )
        )
        res.toList

    private def checkWinningScore(
        board: Array[Array[Int]],
        mark: Array[Array[Boolean]],
        lastNum: Int
    ): Option[Long] =
        val candidates: List[Option[Long]] = (board.indices
            .map(r => getWinningScoreFromRow(board, mark, r, lastNum))
            ++ board.head.indices
                .map(c =>
                    getWinningScoreFromColumn(board, mark, c, lastNum)
                )).toList
        val res: Option[Long] = candidates
            .find((o: Option[Long]) =>
                o match
                    case Some(value) => true
                    case None        => false
            ) match
            case Some(value) => value
            case None        => None
        res

    private def getWinningScoreFromRow(
        board: Array[Array[Int]],
        mark: Array[Array[Boolean]],
        r: Int,
        lastNum: Int
    ): Option[Long] =
        val row = board(r).indices.map(c => (r, c)).toList
        getWinningScore(board, mark, row, lastNum)

    private def getWinningScoreFromColumn(
        board: Array[Array[Int]],
        mark: Array[Array[Boolean]],
        c: Int,
        lastNum: Int
    ): Option[Long] =
        val column = board.indices.map(r => (r, c)).toList
        getWinningScore(board, mark, column, lastNum)

    private def getWinningScore(
        board: Array[Array[Int]],
        mark: Array[Array[Boolean]],
        cells: List[(Int, Int)],
        lastNum: Int
    ): Option[Long] =
        if cells.forall((r, c) => mark(r)(c)) then
            val unmarkedSum = (for
                r <- board.indices
                c <- board.head.indices
            yield (r, c))
                .filter((r, c) => !mark(r)(c))
                .map((r, c) => board(r)(c))
                .sum
            val score = unmarkedSum * lastNum
            Some(score)
        else None
