import scala.io.Source

case class Day01(lines: List[String]):
  // Transform list of "x   y" into a tuple of two lists with all the x's and y's
  private def parse() =
    lines
      .map(_.split(" {3}").map(_.toInt))
      .collect { case Array(x, y) => (x, y) }
      .unzip

  // Compute the sum of the distance between each pair of x and y
  def part1(): Int =
    val (left, right) = parse()
    left.sorted.zip(right.sorted).map((x, y) => (x - y).abs).sum

  // Compute the product of a number in left with the number of times it appears in right
  def part2(): Int =
    val (left, right) = parse()
    left.map(x => x * right.count(_ == x)).sum

case object Day01 extends App {
  val lines = Source.fromResource(s"input1.txt").getLines().toList
  val day = Day01(lines)
  println(s"Part 1: ${day.part1()}")
  println(s"Part 2: ${day.part2()}")
}
