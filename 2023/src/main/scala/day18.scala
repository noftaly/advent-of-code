private case class Hole(location: Location, color: String) extends Tile

private val pattern = "([RDLU]) (\\d+) \\(#(\\w+)\\)".r

def day18(lines: List[String], part: Int = 1): BigInt =
    val instructions = lines
        .map {
            case pattern(direction, amount, color) =>
                if part == 1 then
                    Step(
                        direction match {
                            case "R" => Direction.East
                            case "D" => Direction.South
                            case "L" => Direction.West
                            case "U" => Direction.North
                        },
                        amount.toInt,
                    )
                else
                    val (hexDistance, hexDirection) = color.splitAt(color.length - 1)
                    Step(
                        hexDirection.head match {
                            case '0' => Direction.East
                            case '1' => Direction.South
                            case '2' => Direction.West
                            case '3' => Direction.North
                        },
                        Integer.parseInt(hexDistance.mkString, 16)
                    )
        }

    val outline = instructions.map(instr => BigInt(instr.distance)).sum / 2
    val corners = generateVertices(instructions)

    shoelace(corners) + outline + 1
