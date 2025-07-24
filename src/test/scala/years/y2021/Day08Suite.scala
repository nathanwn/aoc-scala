package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day08Suite extends AnyFunSuite:
    test("Task 1 - Sample") {
        assert(Day08.solve(1, "sample.txt") == 26)
    }
    test("Task 1 - Input") {
        assert(Day08.solve(1, "input.txt") == 539)
    }
    test("Task 2 - Sample") {
        assert(Day08.solve(2, "sample.txt") == 61229)
    }
    test("Task 2 - Input") {
        assert(Day08.solve(2, "input.txt") == 1084606)
    }
