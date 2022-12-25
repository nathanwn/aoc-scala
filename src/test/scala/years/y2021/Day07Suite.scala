package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day07Suite extends AnyFunSuite:
  test("Task 1 - Sample") {
    assert(Day07.solve(1, "sample.txt") == 37)
  }
  test("Task 1 - Input") {
    assert(Day07.solve(1, "input.txt") == 336040)
  }
  test("Task 2 - Sample") {
    assert(Day07.solve(2, "sample.txt") == 168)
  }
  test("Task 2 - Input") {
    assert(Day07.solve(2, "input.txt") == 94813675)
  }
