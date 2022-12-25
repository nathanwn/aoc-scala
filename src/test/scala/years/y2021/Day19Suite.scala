package years.y2021

import org.scalatest.funsuite.AnyFunSuite
import years.y2021

class Day19Suite extends AnyFunSuite:
  test("Task 1 - Sample") {
    assert(Day19.solve(1, "sample.txt") == 79)
  }
  test("Task 1 - Input") {
    assert(Day19.solve(1, "input.txt") == 479)
  }
  test("Task 2 - Sample") {
    assert(Day19.solve(2, "sample.txt") == 3621)
  }
  test("Task 2 - Input") {
    assert(Day19.solve(2, "input.txt") == 13113)
  }
