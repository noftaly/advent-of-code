/**
 * Split a given list at the given predicate. eg: A, B, C, D, C, E; if you split at "C" you get AB, D, E
 */
private def splitListAt[A](list: List[A], predicate: A => Boolean): List[List[A]] =
    list.span(predicate) match {
        case (prefix, suffix) if suffix.nonEmpty => prefix :: splitListAt(suffix.drop(1), predicate)
        case (prefix, _) => List(prefix)
    }

private def nbDifferences(a: Seq[String], b: Seq[String]): Int =
    a
        .zipWithIndex
        .map((str, i) => str.zipWithIndex.count((char, j) => b(i).charAt(j) != char))
        .sum

private def verticalReflection(pattern: Seq[String], tolerance: Int): Int =
    (1 until pattern.head.length)
        .find(sep =>
            val size = Math.min(sep, pattern.head.length - sep)
            val columnsLeft = (sep - size until sep).map(i => pattern.map(_.charAt(i)).mkString)
            val columnsRight = (sep until sep + size).map(i => pattern.map(_.charAt(i)).mkString)
            nbDifferences(columnsLeft.reverse, columnsRight) == tolerance
        )
        .getOrElse(0)

private def horizontalReflection(pattern: Seq[String], tolerance: Int): Int =
    (1 until pattern.size)
        .find(sep =>
            val size = Math.min(sep, pattern.size - sep)
            val rowsAbove = (sep - size until sep).map(pattern(_))
            val rowsBelow = (sep until sep + size).map(pattern(_))
            nbDifferences(rowsAbove.reverse, rowsBelow) == tolerance
        )
        .getOrElse(0)

def day13(lines: List[String], part: Int = 1): BigInt =
    splitListAt[String](lines, _.nonEmpty)
        .map(pattern => verticalReflection(pattern, part - 1) + horizontalReflection(pattern, part - 1) * 100)
        .sum
