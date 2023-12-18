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
    def west(distance: Int = 1): Location = Location(line, column - distance)
    def east(distance: Int = 1): Location = Location(line, column + distance)
    def north(distance: Int = 1): Location = Location(line - distance, column)
    def south(distance: Int = 1): Location = Location(line + distance, column)
    def west: Location = west()
    def east: Location = east()
    def north: Location = north()
    def south: Location = south()

    def neighbors: Set[Location] = Set(west, east, north, south)

    def go(direction: Direction, distance: Int = 1): Location = direction match {
        case Direction.North => north(distance)
        case Direction.South => south(distance)
        case Direction.West => west(distance)
        case Direction.East => east(distance)
    }

    def isPositive: Boolean = line >= 0 && column >= 0
    def isWithin(a: Location, b: Location): Boolean =
        line >= a.line && line < b.line && column >= a.column && column < b.column

case class Step(direction: Direction, distance: Int)

trait Tile:
    def location: Location

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

case class PatternStart[A](index: Int, list: LazyList[A])

/*
 * Returns cycle offset, period
 */
def detectCycle[A](it: Iterator[A], patternRepetitions: Int = 3): Option[(Int, Int)] =
     def hasRepetition(list: LazyList[A], index: Int)(start: PatternStart[A]): Boolean =
         val size = (index - start.index) * patternRepetitions
         list.take(size) == start.list.take(size)

     @tailrec
     def detectCycleRec(list: LazyList[A], seen: Map[A, IndexedSeq[PatternStart[A]]], index: Int): Option[(Int, Int)] =
         if list.isEmpty then
             None
         else
             val element = list.head
             val result = for
                 starts <- seen.get(element)
                 start <- starts.find(hasRepetition(list, index))
             yield (start.index, index - start.index)

             result match {
                 case found @ Some(_) => found
                 case None => detectCycleRec(
                     list.tail,
                     seen + (element -> seen.getOrElse(element, IndexedSeq.empty).appended(PatternStart(index, list))),
                     index + 1
                 )
             }

     detectCycleRec(LazyList.from(it), Map.empty, 0)

def generateVertices(instructions: List[Step]): List[Location] =
    instructions.foldLeft(List(Location(0, 0))) { (vertices, instruction) =>
        val lastLocation = vertices.last
        val newLocation = lastLocation.go(instruction.direction, instruction.distance)
        vertices :+ newLocation
    }

def shoelace(openedCoords: Seq[Location]): BigInt =
    val n = openedCoords.size
    if n < 3 then throw new RuntimeException("Polygons must have at least three vertices")

    val coords = openedCoords ++ Set(openedCoords.head)

    var area = BigInt(0)
    for i <- 0 until n do
        area = area + BigInt(coords(i).line) * BigInt(coords(i + 1).column) - BigInt(coords(i + 1).line) * BigInt(coords(i).column)

    area.abs / 2
