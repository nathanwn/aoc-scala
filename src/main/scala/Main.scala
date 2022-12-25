import years.y2021

@main def main(year: Int, day: Int, task: Int): Unit =
  if !(2015 to 2022 contains year) then
    throw RuntimeException("Invalid year ${year}")
  if !(1 to 25 contains day) then throw RuntimeException("Invalid day ${day}")
  if !(1 to 2 contains task) then throw RuntimeException("Invalid task ${task}")

  println(s"Welcome to day ${day} of ${year}")
  println(s"Current task: ${task}")

  year match
    case 2021 =>
      day match
        case 1  => y2021.Day01.run(task)
        case 2  => throw NotImplementedError()
        case 3  => throw NotImplementedError()
        case 4  => throw NotImplementedError()
        case 5  => y2021.Day05.run(task)
        case 6  => y2021.Day06.run(task)
        case 7  => y2021.Day07.run(task)
        case 8  => y2021.Day08.run(task)
        case 9  => y2021.Day09.run(task)
        case 10 => y2021.Day10.run(task)
        case 11 => y2021.Day11.run(task)
        case 12 => y2021.Day12.run(task)
        case 13 => y2021.Day13.run(task)
        case 14 => y2021.Day14.run(task)
        case 15 => y2021.Day15.run(task)
        case 16 => y2021.Day16.run(task)
        case 17 => y2021.Day17.run(task)
        case 18 => y2021.Day18.run(task)
        case 19 => y2021.Day19.run(task)
        case 20 => y2021.Day20.run(task)
        case 21 => y2021.Day21.run(task)
        case 22 => y2021.Day22.run(task)
        case 23 => y2021.Day23.run(task)
        case 24 => throw NotImplementedError()
        case 25 => throw NotImplementedError()
        case _  => throw RuntimeException(s"Invalid day $day")
    case _ => throw NotImplementedError()
