package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day03Suite extends AnyFunSuite:
    test("Task 1 - Sample") {
        assert(Day03.solve(1, "sample.txt") == 198)
    }
    test("Task 1 - Input") {
        assert(Day03.solve(1, "input.txt") == 1082324)
    }
    test("Task 2 - Sample") {
        assert(Day03.solve(2, "sample.txt") == 230)
    }
    test("Task 2 - Input") {
        assert(Day03.solve(2, "input.txt") == 1353024)
    }
