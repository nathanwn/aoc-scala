package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day04Suite extends AnyFunSuite:
    test("Task 1 - Sample") {
        assert(Day04.solve(1, "sample.txt") == 4512)
    }
    test("Task 1 - Input") {
        assert(Day04.solve(1, "input.txt") == 44088)
    }
    test("Task 2 - Sample") {
        assert(Day04.solve(2, "sample.txt") == 1924)
    }
    test("Task 2 - Input") {
        assert(Day04.solve(2, "input.txt") == 23670)
    }
