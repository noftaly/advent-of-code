private case class Card(value: Int)

private object Card:
    def from(char: Char, useJokers: Boolean): Card = char match {
        case 'A' => Card(14)
        case 'K' => Card(13)
        case 'Q' => Card(12)
        case 'J' => if useJokers then Card(1) else Card(11)
        case 'T' => Card(10)
        case '9' => Card(9)
        case '8' => Card(8)
        case '7' => Card(7)
        case '6' => Card(6)
        case '5' => Card(5)
        case '4' => Card(4)
        case '3' => Card(3)
        case '2' => Card(2)
        case _ => throw new IllegalArgumentException(s"Unknown card symbol: $char")
    }

private enum HandType:
    case FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, OnePair, Distinct

private def getHandType(cards: Seq[Card]): HandType =
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

private case class Hand(cards: Seq[Card], bid: Int):
    val handType: HandType =
        if cards.contains(Card(1)) then
            val possibilities = (1 to 10).map(Card(_)) :++ (12 to 14).map(Card(_))

            possibilities
                .map(possibility =>
                    cards.map(card => if card == Card(1) then possibility else card)
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
        .map(line => Hand(line.head.map(Card.from(_, useJokers)), line.last.toInt))
        .sorted
        .reverse
        .zipWithIndex
        .map((hand, i) => hand.bid * (i + 1))
        .sum
