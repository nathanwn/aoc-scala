package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day22Suite extends AnyFunSuite:
    test("Task 1 - Sample") {
        assert(Day22.solve(1, "sample.txt") == 474140)
    }
    test("Task 1 - Input") {
        assert(Day22.solve(1, "input.txt") == 647062)
    }
    test("Task 2 - Sample") {
        assert(Day22.solve(2, "sample.txt") == 2758514936282235L)
    }
    test("Task 2 - Input") {
        assert(Day22.solve(2, "input.txt") == 1319618626668022L)
    }
