package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day13Suite extends AnyFunSuite:
    test("Task 1 - Sample") {
        assert(Day13.solve(1, "sample.txt") == 17)
    }
    test("Task 1 - Input") {
        assert(Day13.solve(1, "input.txt") == 775)
    }
    test("Task 2 - Sample") {
        assert(Day13.solve(2, "sample.txt") == 16)
    }
    test("Task 2 - Input") {
        assert(Day13.solve(2, "input.txt") == 102)
    }
