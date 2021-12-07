@main def main(day: String): Unit =
  println(s"Welcome to day ${day}")
  day match {
    case "01" => Day01.run()
    case "05" => Day05.run()
    case "06" => Day06.run()
    case "07" => Day07.run()
  }

