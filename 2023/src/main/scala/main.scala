import scala.io.Source

@main def main: Unit =
    val day = 3
    val part = 1

    val bufferedSource = Source.fromResource(s"input$day.txt")
    val lines = bufferedSource.getLines().toList
    bufferedSource.close()

    day match {
        case 1 => println(day1(lines, part))
        case 2 => println(day2(lines, part))
        case 3 => println(day3(lines, part))
    }
