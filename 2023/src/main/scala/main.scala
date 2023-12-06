import scala.io.Source

@main def main: Unit =
    val day = 1
    val part = 1

    val bufferedSource = Source.fromResource(s"input$day.txt")
    val lines = bufferedSource.getLines().toList
    bufferedSource.close()

    day match {
        case 1 => println(day1(lines, part))
    }
