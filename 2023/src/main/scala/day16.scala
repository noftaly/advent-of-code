private enum MirrorInclination:
    case LeanRight, LeanLeft

    def follow(direction: Direction): Direction = this match
        case LeanRight => direction match
            case Direction.North => Direction.East
            case Direction.South => Direction.West
            case Direction.East => Direction.North
            case Direction.West => Direction.South
        case LeanLeft => direction match
            case Direction.North => Direction.West
            case Direction.South => Direction.East
            case Direction.East => Direction.South
            case Direction.West => Direction.North

private enum Orientation:
    case Vertical, Horizontal

private case class Mirror(location: Location, inclination: MirrorInclination) extends Tile
private case class Splitter(location: Location, orientation: Orientation) extends Tile
private case class Void(location: Location) extends Tile

private case class Beam(location: Location, direction: Direction):
    def reflect(tile: Tile): Set[Beam] =
        tile match
            case Mirror(_, inclination) =>
                val newDirection = inclination.follow(direction)
                Set(Beam(location.go(newDirection), newDirection))

            case Splitter(_, Orientation.Vertical) => direction match
                case Direction.North | Direction.South => Set(Beam(location.go(direction), direction))
                case Direction.East | Direction.West => Set(
                    Beam(location.go(Direction.North), Direction.North),
                    Beam(location.go(Direction.South), Direction.South)
                )

            case Splitter(_, Orientation.Horizontal) => direction match
                case Direction.North | Direction.South => Set(
                    Beam(location.go(Direction.East), Direction.East),
                    Beam(location.go(Direction.West), Direction.West)
                )
                case Direction.East | Direction.West => Set(Beam(location.go(direction), direction))

            case Void(_) => Set(Beam(location.go(direction), direction))

def computeEnergy(map: Seq[Seq[Tile]], startBeam: Beam): Int =
    var energized = Set[Location]()
    var visited = Set[Beam]()

    var beams = List(startBeam)
    while beams.nonEmpty do
        beams = beams
            .filter(_.location.isWithin(Location(0, 0), Location(map.size, map.head.size)))
            .flatMap(beam =>
                val tile = map(beam.location.line)(beam.location.column)
                energized = energized + tile.location
                visited = visited ++ beams
                beam.reflect(tile)
            )
            .filterNot(visited.contains)

    energized.size

def day16(lines: List[String], part: Int = 1): Int =
    val map: Seq[Seq[Tile]] = lines
        .zipWithIndex
        .map((line, i) =>
            line
                .zipWithIndex
                .map {
                    case ('/', j) => Mirror(Location(i, j), MirrorInclination.LeanRight)
                    case ('\\', j) => Mirror(Location(i, j), MirrorInclination.LeanLeft)
                    case ('-', j) => Splitter(Location(i, j), Orientation.Horizontal)
                    case ('|', j) => Splitter(Location(i, j), Orientation.Vertical)
                    case ('.', j) => Void(Location(i, j))
                    case _ => throw new RuntimeException()
                }
        )

    if part == 1 then
        computeEnergy(map, Beam(Location(0, 0), Direction.East))
    else
        val top = for i <- map.indices yield Beam(Location(0, i), Direction.South)
        val bottom = for i <- map.indices yield Beam(Location(map.size - 1, i), Direction.North)
        val left = for i <- map.head.indices yield Beam(Location(i, 0), Direction.East)
        val right = for i <- map.head.indices yield Beam(Location(i,  map.head.size - 1), Direction.West)

        val starterBeams = top ++ bottom ++ left ++ right
        starterBeams.map(computeEnergy(map, _)).max
