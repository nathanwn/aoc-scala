package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day17Suite extends AnyFunSuite:
  test("Task 1 - Sample") {
    assert(Day17.solve(1, "sample.txt") == 45)
  }
  test("Task 1 - Input") {
    assert(Day17.solve(1, "input.txt") == 10878)
  }
  test("Task 2 - Sample") {
    assert(Day17.solve(2, "sample.txt") == 112)
  }
  test("Task 2 - Input") {
    assert(Day17.solve(2, "input.txt") == 4716)
  }
