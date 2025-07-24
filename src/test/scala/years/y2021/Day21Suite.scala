package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day21Suite extends AnyFunSuite:
    test("Task 1 - Sample") {
        assert(Day21.solve(1, "sample.txt") == 739785)
    }
    test("Task 1 - Input") {
        assert(Day21.solve(1, "input.txt") == 913560)
    }
    test("Task 2 - Sample") {
        assert(Day21.solve(2, "sample.txt") == 444356092776315L)
    }
    test("Task 2 - Input") {
        assert(Day21.solve(2, "input.txt") == 110271560863819L)
    }
