package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day23Suite extends AnyFunSuite:
  test("Task 1 - Sample") {
    assert(Day23.solve(1, "sample.txt") == 12521)
  }
  test("Task 1 - Input") {
    assert(Day23.solve(1, "input.txt") == 19059)
  }
  test("Task 2 - Sample") {
    assert(Day23.solve(2, "sample.txt") == 44169)
  }
  test("Task 2 - Input") {
    assert(Day23.solve(2, "input.txt") == 48541)
  }
