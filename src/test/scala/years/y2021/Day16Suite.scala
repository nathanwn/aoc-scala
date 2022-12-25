package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day16Suite extends AnyFunSuite:
  test("Task 1 - Sample") {
    assert(Day16.solve(1, "sample.txt") == 11)
  }
  test("Task 1 - Input") {
    assert(Day16.solve(1, "input.txt") == 1038)
  }
  test("Task 2 - Sample") {
    assert(Day16.solve(2, "sample.txt") == 9)
  }
  test("Task 2 - Input") {
    assert(Day16.solve(2, "input.txt") == 246761930504L)
  }
