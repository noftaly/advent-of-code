import scala.collection.mutable

private enum Rock:
    case MovableRock, FixedRock

private def tilted(rocks: Map[Location, Rock], size: (Int, Int), direction: Direction): Map[Location, Rock] =
    implicit val ordering: Ordering[Location] = direction match {
        case Direction.North => Ordering.by[Location, (Int, Int)](location => (location.line, location.column))
        case Direction.South => Ordering.by[Location, (Int, Int)](location => (location.line, location.column)).reverse
        case Direction.West => Ordering.by[Location, (Int, Int)](location => (location.column, location.line))
        case Direction.East => Ordering.by[Location, (Int, Int)](location => (location.column, location.line)).reverse
    }

    val tiltedRocks = mutable.Map[Location, Rock]()
    val bounds = (Location(0, 0), Location(size._1, size._2))

    rocks
        .toList
        .sortBy(_._1)
        .map {
            case (location, Rock.FixedRock) => tiltedRocks.addOne(location -> Rock.FixedRock)
            case (location, Rock.MovableRock) =>
                var loc = location
                while !tiltedRocks.contains(loc.go(direction)) && loc.go(direction).isWithin.tupled(bounds) do
                    loc = loc.go(direction)
                tiltedRocks.addOne(loc -> Rock.MovableRock)
        }

    tiltedRocks.toMap

def day14(lines: List[String], part: Int = 1): Int =
    val rocks: Map[Location, Rock] = lines
        .zipWithIndex
        .flatMap((line, i) => line
            .zipWithIndex
            .map {
                case ('#', j) => Some(Location(i, j) -> Rock.FixedRock)
                case ('O', j) => Some(Location(i, j) -> Rock.MovableRock)
                case _ => None
            })
        .filter(_.isDefined)
        .map(_.get)
        .toMap

    val size = (lines.head.length, lines.size)

    def computeLoad(rocks: Map[Location, Rock]): Int = rocks
        .filter(_._2 == Rock.MovableRock)
        .keys.toList
        .map(pos => lines.size - pos.line)
        .sum

    if part == 1 then
        computeLoad(tilted(rocks, size, Direction.North))
    else
        def cycle(rocks: Map[Location, Rock]): Map[Location, Rock] =
            tilted(tilted(tilted(tilted(rocks, size, Direction.North), size, Direction.West), size, Direction.South), size, Direction.East)

        val (start, period) = detectCycle(Iterator.iterate(rocks)(cycle)) match {
            case Some((start, period)) => (start, period)
            case _ => throw new RuntimeException
        }

        // Simplification taken from https://github.com/kbielefe/advent-of-code/blob/0001403/2023/src/main/scala/14.scala#L32
        val offset = (1_000_000_000L - start) % period + start
        computeLoad(Iterator.iterate(rocks)(cycle).drop(offset.toInt).next)
