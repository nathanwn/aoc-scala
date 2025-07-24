package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day14Suite extends AnyFunSuite:
    test("Task 1 - Sample") {
        assert(Day14.solve(1, "sample.txt") == 1588)
    }
    test("Task 1 - Input") {
        assert(Day14.solve(1, "input.txt") == 3230)
    }
    test("Task 2 - Sample") {
        assert(Day14.solve(2, "sample.txt") == 2188189693529L)
    }
    test("Task 2 - Input") {
        assert(Day14.solve(2, "input.txt") == 3542388214529L)
    }
