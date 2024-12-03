import scala.io.Source

case class Day02(lines: List[String]):
  private def parse() = lines.map(_.split(" ").map(_.toInt).toList)

  // A line is safe if it is only ever-increasing or decreasing, and by at most 3 and at least 1.
  private def checkPairs(values: List[Int]): Boolean =
    val differences = values.sliding(2).map { case List(x, y) => x - y }.toVector
    differences.forall(diff => diff.sign == differences.head.sign && diff.abs <= 3 && diff.abs >= 1)

  // Count all the line which are safe
  def part1(): Int = parse().count(checkPairs)

  // Same as part 1, but if removing a single element from an unsafe line makes it safe, then it is considered safe
  def part2(): Int = parse().count(line =>
    checkPairs(line) ||
      // Check all the possible lines by removing a single element.
      // Optimization: have checkPairs return the faulty index, so we don't have to check all the lines
      line.indices
        .map(i => line.slice(0, i) ++ line.slice(i + 1, line.length))
        .exists(checkPairs)
  )

case object Day02 extends App {
  val lines = Source.fromResource(s"input2.txt").getLines().toList
  val day = Day02(lines)
  println(s"Part 1: ${day.part1()}")
  println(s"Part 2: ${day.part2()}")
}
