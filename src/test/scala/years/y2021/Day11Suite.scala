package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day11Suite extends AnyFunSuite:
    test("Task 1 - Sample") {
        assert(Day11.solve(1, "sample.txt") == 1656)
    }
    test("Task 1 - Input") {
        assert(Day11.solve(1, "input.txt") == 1749)
    }
    test("Task 2 - Sample") {
        assert(Day11.solve(2, "sample.txt") == 195)
    }
    test("Task 2 - Input") {
        assert(Day11.solve(2, "input.txt") == 285)
    }
