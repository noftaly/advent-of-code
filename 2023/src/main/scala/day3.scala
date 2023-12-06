private case class SpanLocation(line: Int, column: (Int, Int))
private case class Location(line: Int, column: Int)

private sealed trait Token {
    def location: Location
    def representation: Char
}

private case class NumberToken(location: Location, representation: Char, number: Int) extends Token
private case class SymbolToken(location: Location, representation: Char) extends Token
private case class VoidToken(location: Location, representation: Char) extends Token

private case class NumberWord(location: SpanLocation, tokens: Seq[NumberToken], number: Int)

private class TokenMap(lines: List[String]):
    val map: List[List[Token]] = lines
        .zipWithIndex
        .map { case (line, i) =>
            line
                .zipWithIndex
                .map { case (char, j) =>
                    char match {
                        case char if char.isDigit => NumberToken(Location(i, j), char, char.asDigit)
                        case char@'.' => VoidToken(Location(i, j), char)
                        case symbol => SymbolToken(Location(i, j), symbol)
                    }
                }
                .toList
        }

    val words: List[NumberWord] = {
        var tmpWords = List[NumberWord]()
        for line <- map do
            var i = 0
            while i < line.size do
                line(i) match {
                    case NumberToken(location, representation, number) =>
                        var tokens = List[NumberToken](line(i).asInstanceOf[NumberToken])
                        i += 1
                        while line.lift(i).isDefined && line(i).isInstanceOf[NumberToken] do
                            tokens :+= line(i).asInstanceOf[NumberToken]
                            i += 1

                        val span = SpanLocation(
                            tokens.head.location.line,
                            (tokens.head.location.column, tokens.last.location.column)
                        )
                        val num = tokens.map(_.number).mkString("").toInt
                        tmpWords :+= NumberWord(span, tokens, num)
                    case _ => i += 1
                }
        tmpWords
    }

    def tokensAround(location: SpanLocation): List[Token] =
        val colMin = Math.max(location.column._1 - 1, 0)
        val colMax = Math.min(location.column._2 + 1, map.head.size - 1)
        val lineMin = Math.max(location.line - 1, 0)
        val lineMax = Math.min(location.line + 1, map.size - 1)

        map
            .flatten
            .filter(tok =>
                tok.location.column >= colMin
                    && tok.location.column <= colMax
                    && tok.location.line >= lineMin
                    && tok.location.line <= lineMax
                    && (
                    if (tok.location.line == location.line)
                        tok.location.column < location.column._1 || tok.location.column > location.column._2
                    else true
                    )
            )

    def isEnginePart(location: SpanLocation): Boolean =
        tokensAround(location).exists {
            case SymbolToken(_, _) => true
            case _ => false
        }

def day3(lines: List[String], part: Int = 1): Int =
    val tokenMap = TokenMap(lines)

    if part == 1 then
        tokenMap.words
            .filter(word => tokenMap.isEnginePart(word.location))
            .map(_.number)
            .sum
    else
        tokenMap.words
            // Get all the stars around each word
            .map { word =>
                word -> tokenMap.tokensAround(word.location)
                    .find {
                        case SymbolToken(_, '*') => true
                        case _ => false
                    }
            }
            .filter(_._2.isDefined)
            // Group by the star so we get our clusters
            .groupBy(_._2.get)
            .view
            .mapValues(_.map(_._1))
            // Remove clusters where the size != 2
            .filter(_._2.size == 2)
            // Compute the "gear ratio"
            .mapValues(cluster => cluster.map(_.number))
            .mapValues(cluster => cluster.product)
            .values
            // Sum the gear ratios
            .sum
