package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day06Suite extends AnyFunSuite:
  test("Task 1 - Sample") {
    assert(Day06.solve(1, "sample.txt") == 5934)
  }
  test("Task 1 - Input") {
    assert(Day06.solve(1, "input.txt") == 358214)
  }
  test("Task 2 - Sample") {
    assert(Day06.solve(2, "sample.txt") == 26984457539L)
  }
  test("Task 2 - Input") {
    assert(Day06.solve(2, "input.txt") == 1622533344325L)
  }
