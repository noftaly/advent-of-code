import scala.io.Source
import scala.util.matching.Regex

val mulPattern: Regex = "mul\\((\\d+),(\\d+)\\)".r

case class Day03(lines: List[String]):
  private def computeSum(values: Seq[String]) =
    values
      .flatMap(mulPattern.findAllMatchIn)
      .map(m => m.group(1).toInt * m.group(2).toInt)
      .sum

  // Multiply all mul(X,Y) there is, and sum them
  def part1(): Int = computeSum(lines)

  // Take into account "do() and "don't()" which enable/disable further "mul()" calls
  def part2(): Int = computeSum(
    lines
      .mkString
      .split("do\\(\\)")
      .map(_.split("don't\\(\\)").head)
  )

case object Day03 extends App {
  val lines = Source.fromResource(s"input3.txt").getLines().toList
  val day = Day03(lines)
  println(s"Part 1: ${day.part1()}")
  println(s"Part 2: ${day.part2()}")
}
