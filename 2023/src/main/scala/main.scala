import scala.io.Source

@main def main: Unit =
    val day = 11
    val part = 1

    val bufferedSource = Source.fromResource(s"input$day.txt")
    val lines = bufferedSource.getLines().toList
    bufferedSource.close()

    day match {
        case 1 => println(day1(lines, part))
        case 2 => println(day2(lines, part))
        case 3 => println(day3(lines, part))
        case 4 => println(day4(lines, part))
        case 6 => println(day6(lines, part))
        case 7 => println(day7(lines, part))
        case 8 => println(day8(lines, part))
        case 9 => println(day9(lines, part))
        case 10 => println(day10(lines, part))
        case 11 => println(day11(lines, part))
    }
