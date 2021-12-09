import Day08.{Entry, parseMask}
import lib.*

object Day08 extends AocDay[List[Entry], Int]("data/day08") :
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
    val uniqueCounts = List[Int](2, 4, 3, 7)
    entries.map(entry =>
      entry.display.map(mask =>
        if uniqueCounts.contains(Integer.bitCount(mask)) then 1 else 0
      ).sum
    ).sum

  def solve2(entries: List[Entry]): Int =
    entries.map(solveEntry).sum

  def solveEntry(entry: Entry): Int =
    // Find the correct mapping f(c) where c is a segment
    val f = solveMapping(entry)
    entry.display.map(encodedMask =>
      digitMasks.indexOf(decode(f)(encodedMask))
    ).reduce((lhs, rhs) => lhs * 10 + rhs)

  def solveMapping(entry: Entry): Array[Int] =
    val maskSet = digitMasks.toSet
    val allMappings = range(0, 6).permutations.map(_.toArray).toList
    val mapping = allMappings.find(f =>
      entry.signals
        .map(decode(f))
        .forall(decodedMask => digitMasks.indexOf(decodedMask) != -1)
    )
    mapping match {
      case Some(f) => f
      case None => Array.ofDim(0)
    }

  def decode(f: Array[Int])(encodedMask: Int): Int =
    range(0, 6).map(c =>
      if (encodedMask & (1 << f(c))) != 0 then 1 << c else 0
    ).sum