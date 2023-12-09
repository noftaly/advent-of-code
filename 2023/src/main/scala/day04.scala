private case class GamblingCard(id: Int, winning: Set[Int], ours: Set[Int]):
    val correct: Int = (ours & winning).size

def day4(lines: List[String], part: Int = 1): Int =
    val pattern = "Card\\s+(\\d+):\\s+(.*) \\|\\s+(.*)".r

    val cards = lines.map {
        case pattern(id, winning, ours) => GamblingCard(
            id.toInt,
            winning.split("\\s+").map(_.toInt).toSet,
            ours.split("\\s+").map(_.toInt).toSet)
    }

    if (part == 1)
        cards
            .filter(_.correct > 0)
            .map(card => Math.pow(2, card.correct - 1))
            .sum
            .intValue
    else
        val init = cards.map(_.id -> 1).toMap

        cards
            .foldLeft(init) { (copies, card) =>
                // Loop from my card to the nth correct ones after
                (1 to card.correct).map(_ + card.id)
                    // For each of them, get their values and add our value to id
                    .foldLeft(copies)((copies, copy) => copies.updated(copy, copies(copy) + copies(card.id)))
            }
            .values
            .sum
