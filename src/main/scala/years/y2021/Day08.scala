package years.y2021

import Day08.{Entry, parseMask}
import lib.day.AocDay

object Day08 extends AocDay[List[Entry], Int]:
    val digitMasks: Array[Int] = Array.ofDim[Int](10)
    digitMasks(0) = parseMask("abcefg")
    digitMasks(1) = parseMask("cf")
    digitMasks(2) = parseMask("acdeg")
    digitMasks(3) = parseMask("acdfg")
    digitMasks(4) = parseMask("bcdf")
    digitMasks(5) = parseMask("abdfg")
    digitMasks(6) = parseMask("abdefg")
    digitMasks(7) = parseMask("acf")
    digitMasks(8) = parseMask("abcdefg")
    digitMasks(9) = parseMask("abcdfg")

    class Entry(val signals: List[Int], val display: List[Int])

    def parseMask(s: String): Int =
        s.map(c => 1 << (c - 'a')).sum

    def parseEntry(line: String): Entry =
        val tokens = line.split(' ').toList
        val signals: List[Int] = tokens.take(10).map(parseMask)
        val display: List[Int] = tokens.drop(11).map(parseMask)
        Entry(signals, display)

    def parse(input: String): List[Entry] =
        input.split('\n').toList.map(parseEntry)

    def solve1(entries: List[Entry]): Int =
        entries.map(solveEntry1).sum

    def solve2(entries: List[Entry]): Int =
        entries.map(solveEntry2).sum

    def solveEntry1(entry: Entry): Int =
        val f = solveMapping(entry)
        entry.display
            .map(encodedMask =>
                val digit = digitMasks.indexOf(decode(f)(encodedMask))
                if List(1, 4, 7, 8).contains(digit) then 1 else 0
            )
            .sum

    def solveEntry2(entry: Entry): Int =
        val f = solveMapping(entry)
        entry.display
            .map(encodedMask =>
                val digit = digitMasks.indexOf(decode(f)(encodedMask));
                digit
            )
            .reduce((lhs, rhs) => lhs * 10 + rhs)

    def solveMapping(entry: Entry): Array[Int] =
        // Find the correct mapping f(c) where c is a segment
        val maskSet = digitMasks.toSet
        val allMappings = (0 to 6).permutations.map(_.toArray).toList
        val mapping = allMappings.find(f =>
            entry.signals
                .map(decode(f))
                .forall(decodedMask => digitMasks.indexOf(decodedMask) != -1)
        )
        mapping match
            case Some(f) => f
            case None    => Array.ofDim(0)

    def decode(f: Array[Int])(encodedMask: Int): Int =
        (0 to 6)
            .map(c => if (encodedMask & (1 << f(c))) != 0 then 1 << c else 0)
            .sum
