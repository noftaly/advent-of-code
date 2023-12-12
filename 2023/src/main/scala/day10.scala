private case class TileLocation(line: Int, column: Int)

private enum TileDirection:
    case North, South, East, West

    def opposite: TileDirection = this match
        case North => South
        case South => North
        case East => West
        case West => East

private object TileDirection:
    val combinations: Seq[(TileDirection, TileDirection)] =
        for {
            (d1, i) <- TileDirection.values.zipWithIndex
            (d2, j) <- TileDirection.values.zipWithIndex
            if d1 != d2 && i < j // Ensuring only one of the reversed pairs is included
        } yield (d1, d2)

import TileDirection._

private sealed trait Tile {
    def location: TileLocation
}
private case class Ground(location: TileLocation) extends Tile
private case class Start(location: TileLocation) extends Tile
private case class Pipe(location: TileLocation, direction: (TileDirection, TileDirection)) extends Tile

private case class Position(tile: Tile, direction: TileDirection)

private class Grid(lines: Seq[String]):
    val map: Seq[Seq[Tile]] = lines
        .zipWithIndex
        .map((line, i) =>
            line
                .zipWithIndex
                .map {
                    case ('.', j) => Ground(TileLocation(i, j))
                    case ('S', j) => Start(TileLocation(i, j))
                    case ('|', j) => Pipe(TileLocation(i, j), (North, South))
                    case ('-', j) => Pipe(TileLocation(i, j), (East, West))
                    case ('J', j) => Pipe(TileLocation(i, j), (North, West))
                    case ('7', j) => Pipe(TileLocation(i, j), (South, West))
                    case ('F', j) => Pipe(TileLocation(i, j), (South, East))
                    case ('L', j) => Pipe(TileLocation(i, j), (North, East))
                    case (tile, j) => throw new IllegalArgumentException(s"Unexpected tile $tile at $i,$j")
                }
        )

    val start: Tile = map
        .flatten
        .find {
            case tile: Start => true
            case _ => false
        }
        .get

    val startPipe: Pipe =
         TileDirection
             .combinations
             .map(Pipe(start.location, _))
             .filter(getConnectedPipes(_).size == 2)
             .head

    def getNeighbors(location: TileLocation): Seq[Position] =
        map
            .flatten
            .map(tile =>
                if tile.location.column == location.column - 1 && tile.location.line == location.line then Some(Position(tile, West))
                else if tile.location.column == location.column + 1 && tile.location.line == location.line then Some(Position(tile, East))
                else if tile.location.line == location.line - 1 && tile.location.column == location.column then Some(Position(tile, North))
                else if tile.location.line == location.line + 1 && tile.location.column == location.column then Some(Position(tile, South))
                else None
            )
            .filter(_.isDefined)
            .map(_.get)

    def getConnectedPipes(pipe: Pipe): Seq[Pipe] =
        getNeighbors(pipe.location)
            .filter {
                case Position(tile: Pipe, direction) =>
                    val weAreConnected = pipe.direction.productIterator.contains(direction)
                    val theyAreConnected = tile.direction.productIterator.contains(direction.opposite)
                    weAreConnected && theyAreConnected
                case _ => false
            }
            .map(_.tile.asInstanceOf[Pipe])

    def getNextPipe(sequence: Seq[Pipe]): Option[Pipe] =
        getConnectedPipes(sequence.last)
            .find(!sequence.contains(_))

def day10(lines: List[String], part: Int = 1): Int =
    val grid = Grid(lines)

    if part == 1 then
        // Method 1: Have to cursors travel their own way, stop when they meet
        // var paths = grid.getConnectedPipes(grid.startPipe).map(List(_))
        // while paths.map(_.last).distinct.size != 1 do
        //     paths = paths.map(path =>
        //         grid.getNextPipe(path).map(path :+ _).getOrElse(path)
        //     )
        // paths.head.size

        // Method 2: Have one cursor do the whole loop, then divide by 2
        var pipes: Seq[Option[Pipe]] = grid.getConnectedPipes(grid.startPipe).take(1).map(Some(_))
        while pipes.last.isDefined do
            pipes :+= grid.getNextPipe(pipes.map(_.get))

        pipes.size / 2
    else
        0