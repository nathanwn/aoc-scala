package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day09Suite extends AnyFunSuite:
  test("Task 1 - Sample") {
    assert(Day09.solve(1, "sample.txt") == 15)
  }
  test("Task 1 - Input") {
    assert(Day09.solve(1, "input.txt") == 468)
  }
  test("Task 2 - Sample") {
    assert(Day09.solve(2, "sample.txt") == 1134)
  }
  test("Task 2 - Input") {
    assert(Day09.solve(2, "input.txt") == 1280496)
  }
