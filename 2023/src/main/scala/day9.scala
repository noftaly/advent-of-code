import scala.annotation.tailrec

@tailrec
def differencesUntilZero(steps: List[List[Int]]): List[List[Int]] =
    if steps.last.exists(_ != 0) then differencesUntilZero(
        steps :+ steps.last.sliding(2).map { case List(a, b) => b - a }.toList
    )
    else steps

def day9(lines: List[String], part: Int = 1): Int =
    val sequences = lines
        .map(line => List(line.split(" ").map(_.toInt).toList))
        .map(differencesUntilZero)

    if part == 1 then
        sequences
            .map(_.foldRight(0) { case (list, acc) => acc + list.last })
            .sum
    else
        sequences
            .map(_.foldRight(0) { case (list, acc) => list.head - acc })
            .sum
