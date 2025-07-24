package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day01Suite extends AnyFunSuite:
    test("Task 1 - Sample") {
        assert(Day01.solve(1, "sample.txt") == 7)
    }
    test("Task 1 - Input") {
        assert(Day01.solve(1, "input.txt") == 1390)
    }
    test("Task 2 - Sample") {
        assert(Day01.solve(2, "sample.txt") == 5)
    }
    test("Task 2 - Input") {
        assert(Day01.solve(2, "input.txt") == 1457)
    }
