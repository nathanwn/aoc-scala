package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day20Suite extends AnyFunSuite:
  test("Task 1 - Sample") {
    assert(Day20.solve(1, "sample.txt") == 35)
  }
  test("Task 1 - Input") {
    assert(Day20.solve(1, "input.txt") == 4964)
  }
  test("Task 2 - Sample") {
    assert(Day20.solve(2, "sample.txt") == 3351)
  }
  test("Task 2 - Input") {
    assert(Day20.solve(2, "input.txt") == 13202)
  }
