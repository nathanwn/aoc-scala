package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day05Suite extends AnyFunSuite:
    test("Task 1 - Sample") {
        assert(Day05.solve(1, "sample.txt") == 5)
    }
    test("Task 1 - Input") {
        assert(Day05.solve(1, "input.txt") == 6113)
    }
    test("Task 2 - Sample") {
        assert(Day05.solve(2, "sample.txt") == 12)
    }
    test("Task 2 - Input") {
        assert(Day05.solve(2, "input.txt") == 20373)
    }
