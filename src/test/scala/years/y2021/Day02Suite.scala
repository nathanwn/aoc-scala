package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day02Suite extends AnyFunSuite:
    test("Task 1 - Sample") {
        assert(Day02.solve(1, "sample.txt") == 150)
    }
    test("Task 1 - Input") {
        assert(Day02.solve(1, "input.txt") == 1840243)
    }
    test("Task 2 - Sample") {
        assert(Day02.solve(2, "sample.txt") == 900)
    }
    test("Task 2 - Input") {
        assert(Day02.solve(2, "input.txt") == 1727785422)
    }
