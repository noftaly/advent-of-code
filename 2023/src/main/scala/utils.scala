import scala.annotation.tailrec

def findFirstAmong(str: String, occurrences: Seq[String]): String =
    var min = Int.MaxValue
    var minValue = ""

    for word <- occurrences do
        val a = str.indexOf(word)
        if a != -1 && a < min then
            min = a
            minValue = word

    minValue

def findLastAmong(str: String, occurrences: Seq[String]): String =
    var max = Int.MinValue
    var maxValue = ""

    for word <- occurrences do
        val a = str.lastIndexOf(word)
        if a != -1 && a > max then
            max = a
            maxValue = word

    maxValue

case class Span(line: Int, column: (Int, Int))
case class Location(line: Int, column: Int):
    def west: Location = Location(line, column - 1)
    def east: Location = Location(line, column + 1)
    def north: Location = Location(line - 1, column)
    def south: Location = Location(line + 1, column)

    def go(direction: Direction): Location = direction match {
        case Direction.North => north
        case Direction.South => south
        case Direction.West => west
        case Direction.East => east
    }

    def isPositive: Boolean = line >= 0 && column >= 0
    def isWithin(a: Location, b: Location): Boolean =
        line >= a.line && line < b.line && column >= a.column && column < b.column

enum Direction:
    case North, South, East, West

    def opposite: Direction = this match
        case North => South
        case South => North
        case East => West
        case West => East

object Direction:
    val combinations: Seq[(Direction, Direction)] =
        for {
            (d1, i) <- Direction.values.zipWithIndex
            (d2, j) <- Direction.values.zipWithIndex
            if d1 != d2 && i < j // Ensuring only one of the reversed pairs is included
        } yield (d1, d2)

@tailrec
def gcd(a: BigInt, b: BigInt): BigInt =
    if (b == 0) a.abs
    else gcd(b, a % b)

def lcm(list: Seq[BigInt]): BigInt =
    list.foldLeft(BigInt(1))((a, b) => (a / gcd(a, b)) * b)

def isBetween(num: Int, a: Int, b: Int): Boolean =
    num < Math.max(a, b) && num > Math.min(a, b)

/**
 * Split a given list at the given predicate. eg: A, B, C, D, C, E; if you split at "C" you get AB, D, E
 */
def splitListAt[A](list: List[A], predicate: A => Boolean): List[List[A]] =
    list.span(predicate) match {
        case (prefix, suffix) if suffix.nonEmpty => prefix :: splitListAt(suffix.drop(1), predicate)
        case (prefix, _) => List(prefix)
    }

def nbDifferences(a: Seq[String], b: Seq[String]): Int =
    a
        .zipWithIndex
        .map((str, i) => str.zipWithIndex.count((char, j) => b(i).charAt(j) != char))
        .sum
