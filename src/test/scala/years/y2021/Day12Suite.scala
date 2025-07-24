package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day12Suite extends AnyFunSuite:
    test("Task 1 - Sample") {
        assert(Day12.solve(1, "sample.txt") == 10)
    }
    test("Task 1 - Input") {
        assert(Day12.solve(1, "input.txt") == 5576)
    }
    test("Task 2 - Sample") {
        assert(Day12.solve(2, "sample.txt") == 36)
    }
    test("Task 2 - Input") {
        assert(Day12.solve(2, "input.txt") == 152837)
    }
