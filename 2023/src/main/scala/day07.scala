private case class PlayingCard(value: Int)

private object PlayingCard:
    def from(char: Char, useJokers: Boolean): PlayingCard = char match {
        case 'A' => PlayingCard(14)
        case 'K' => PlayingCard(13)
        case 'Q' => PlayingCard(12)
        case 'J' => if useJokers then PlayingCard(1) else PlayingCard(11)
        case 'T' => PlayingCard(10)
        case '9' => PlayingCard(9)
        case '8' => PlayingCard(8)
        case '7' => PlayingCard(7)
        case '6' => PlayingCard(6)
        case '5' => PlayingCard(5)
        case '4' => PlayingCard(4)
        case '3' => PlayingCard(3)
        case '2' => PlayingCard(2)
        case _ => throw new IllegalArgumentException(s"Unknown card symbol: $char")
    }

private enum HandType:
    case FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, OnePair, Distinct

private def getHandType(cards: Seq[PlayingCard]): HandType =
    val groups = cards
        .groupBy(identity)
        .map(_._2.size)
        .toList
        .sorted

    groups match {
        case List(5) => HandType.FiveOfAKind
        case List(1, 4) => HandType.FourOfAKind
        case List(2, 3) => HandType.FullHouse
        case List(1, 1, 3) => HandType.ThreeOfAKind
        case List(1, 2, 2) => HandType.TwoPair
        case List(1, 1, 1, 2) => HandType.OnePair
        case _ => HandType.Distinct
    }

private case class Hand(cards: Seq[PlayingCard], bid: Int):
    val handType: HandType =
        if cards.contains(PlayingCard(1)) then
            val possibilities = (1 to 10).map(PlayingCard(_)) :++ (12 to 14).map(PlayingCard(_))

            possibilities
                .map(possibility =>
                    cards.map(card => if card == PlayingCard(1) then possibility else card)
                )
                .distinct
                .map(getHandType)
                .minBy(_.ordinal)
        else
            getHandType(cards)

    infix def handSorter(other: Hand): Boolean =
        if handType == other.handType then
            cards
                .zip(other.cards)
                .map(_.value - _.value)
                .find(_ != 0)
                .get > 0
        else
            handType.ordinal < other.handType.ordinal

def day7(lines: List[String], part: Int = 1): Int =
    val useJokers = part == 2

    implicit val ord: Ordering[Hand] = Ordering.fromLessThan(_ handSorter _)

    lines
        .map(_.split(" "))
        .map(line => Hand(line.head.map(PlayingCard.from(_, useJokers)), line.last.toInt))
        .sorted
        .reverse
        .zipWithIndex
        .map((hand, i) => hand.bid * (i + 1))
        .sum
