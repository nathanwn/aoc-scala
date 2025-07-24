package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day15Suite extends AnyFunSuite:
    test("Task 1 - Sample") {
        assert(Day15.solve(1, "sample.txt") == 40)
    }
    test("Task 1 - Input") {
        assert(Day15.solve(1, "input.txt") == 702)
    }
    test("Task 2 - Sample") {
        assert(Day15.solve(2, "sample.txt") == 315)
    }
    test("Task 2 - Input") {
        assert(Day15.solve(2, "input.txt") == 2955)
    }
