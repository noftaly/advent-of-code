private case class SpanLocation(line: Int, column: (Int, Int))
private case class TokenLocation(line: Int, column: Int):
    val west: TokenLocation = TokenLocation(line, column - 1)

private sealed trait Token {
    def location: TokenLocation
    def representation: Char
}

private case class NumberToken(location: TokenLocation, representation: Char, number: Int) extends Token
private case class SymbolToken(location: TokenLocation, representation: Char) extends Token
private case class VoidToken(location: TokenLocation, representation: Char) extends Token

private case class NumberWord(location: SpanLocation, tokens: Seq[NumberToken], number: Int)

private class TokenMap(lines: List[String]):
    val map: List[List[Token]] = lines
        .zipWithIndex
        .map { (line, i) =>
            line
                .zipWithIndex
                .map { (char, j) =>
                    char match {
                        case char if char.isDigit => NumberToken(TokenLocation(i, j), char, char.asDigit)
                        case char@'.' => VoidToken(TokenLocation(i, j), char)
                        case symbol => SymbolToken(TokenLocation(i, j), symbol)
                    }
                }
                .toList
        }

    private def isNumberToken(location: TokenLocation): Boolean =
        map
            .lift(location.line)
            .flatMap(_.lift(location.column)) match {
                case Some(NumberToken(_, _, _)) => true
                case _ => false
            }

    val words: List[NumberWord] =
        map
            .flatten
            .filter {
                // If our left neighbor is a number token, then we are not at the beginning of a new word so we skip
                case NumberToken(location, _, _) => !isNumberToken(location.west)
                case _ => false
            }
            .map { case NumberToken(TokenLocation(line, col), _, _) =>
                val upTo = Iterator.iterate(col)(_ + 1)
                    .takeWhile(idx => isNumberToken(TokenLocation(line, idx)))
                    .toList
                    .last

                val span = SpanLocation(line, (col, upTo))
                val tokens = map(line).slice(col, upTo + 1).map(_.asInstanceOf[NumberToken])
                val num = tokens.map(_.number).mkString.toInt

                NumberWord(span, tokens, num)
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
            .map(word =>
                word -> tokenMap.tokensAround(word.location)
                    .find {
                        case SymbolToken(_, '*') => true
                        case _ => false
                    }
            )
            .filter((word, starAround) => starAround.isDefined)
            .groupBy((word, starAround) => starAround.get)       // Group by the star so we get our clusters
            .values
            .map(_.map(_._1.number)) // Get the number value of each word
            .filter(cluster => cluster.size == 2)      // Remove clusters where the size != 2
            .map(cluster => cluster.product)          // Compute the "gear ratio"
            .sum
