package years.y2021

import lib.day.AocDay

import scala.annotation.tailrec

object Day03 extends AocDay[List[Array[Int]], Long]:
    def parse(text: String): List[Array[Int]] =
        text.linesIterator
            .map((line: String) => line.map(c => c - '0').toArray)
            .toList

    def solve1(data: List[Array[Int]]): Long =
        val binNumCount = data.length
        val binNumLength = data.head.length
        val gammaBits: Array[Int] = (0 until binNumLength)
            .map(position => getMostCommonBitAtPosition(data, position))
            .toArray
        val epsilonBits: Array[Int] = (0 until binNumLength)
            .map(position => getLeastCommonBitAtPosition(data, position))
            .toArray
        val gamma = toDecimal(gammaBits)
        val epsilon = toDecimal(epsilonBits)
        gamma * epsilon

    def solve2(data: List[Array[Int]]): Long =
        val binNumCount = data.length
        val binNumLength = data.head.length
        val o2_rating = toDecimal(reduce(data, 0, true).head)
        val co2_rating = toDecimal(reduce(data, 0, false).head)
        o2_rating * co2_rating

    @tailrec
    private def reduce(
        data: List[Array[Int]],
        position: Int,
        takeMostCommon: Boolean
    ): List[Array[Int]] =
        if data.length == 1 then data
        else
            val newData =
                if takeMostCommon then
                    val mostCommonBit =
                        getMostCommonBitAtPosition(data, position)
                    data.filter(binNum => binNum(position) == mostCommonBit)
                else
                    val leastCommonBit =
                        getLeastCommonBitAtPosition(data, position)
                    data.filter(binNum => binNum(position) == leastCommonBit)
            reduce(newData, position + 1, takeMostCommon)

    private def getMostCommonBitAtPosition(
        data: List[Array[Int]],
        position: Int
    ): Int =
        val bitCountPerPosition = getBitCountPerPosition(data, position)
        if bitCountPerPosition(0) <= bitCountPerPosition(1) then 1
        else 0

    private def getLeastCommonBitAtPosition(
        data: List[Array[Int]],
        position: Int
    ): Int =
        val bitCountPerPosition = getBitCountPerPosition(data, position)
        if bitCountPerPosition(0) <= bitCountPerPosition(1) then 0
        else 1

    private def getBitCountPerPosition(
        data: List[Array[Int]],
        position: Int
    ): Map[Int, Int] =
        data.map(binNum => binNum(position))
            .groupBy(identity)
            .view
            .mapValues(_.length)
            .toMap

    private def toDecimal(bits: Array[Int]): Long =
        var res = 0
        bits.foreach(bit =>
            res = res * 2
            res += bit
        )
        res
