private case class Galaxy(line: Int, column: Int):
    infix def distanceFrom(other: Galaxy): Int =
        (other.line - line).abs + (other.column - column).abs

def day11(lines: List[String], part: Int = 1): BigInt =
    val galaxies = lines
        .zipWithIndex
        .flatMap((line, i) => line
            .zipWithIndex
            .map {
                case ('#', j) => Some(Galaxy(i, j))
                case _ => None
            })
        .filter(_.isDefined)
        .map(_.get)

    val emptyRowIndices = lines
        .zipWithIndex
        .filter(_._1.forall(_ == '.'))
        .map(_._2)

    val emptyColIndices = lines
        .transpose
        .zipWithIndex
        .filter(_._1.forall(_ == '.'))
        .map(_._2)

    val factor = BigInt(if part == 1 then 1 else 999_999)

    galaxies
        .combinations(2)
        .map(pair =>
            val Seq(g1, g2) = pair: @unchecked
            val distance = BigInt(g1 distanceFrom g2)
            val expansionRow = emptyRowIndices.count(isBetween(_, g1.line, g2.line))
            val expansionCol = emptyColIndices.count(isBetween(_, g1.column, g2.column))
            distance + BigInt(expansionRow) * factor + BigInt(expansionCol) * factor
        )
        .sum
