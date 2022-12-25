package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day10Suite extends AnyFunSuite:
  test("Task 1 - Sample") {
    assert(Day10.solve(1, "sample.txt") == 26397L)
  }
  test("Task 1 - Input") {
    assert(Day10.solve(1, "input.txt") == 464991L)
  }
  test("Task 2 - Sample") {
    assert(Day10.solve(2, "sample.txt") == 288957L)
  }
  test("Task 2 - Input") {
    assert(Day10.solve(2, "input.txt") == 3662008566L)
  }
