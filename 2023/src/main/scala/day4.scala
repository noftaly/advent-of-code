private case class GamblingCard(id: Int, winning: Seq[Int], ours: Seq[Int])

private def correct(card: GamblingCard): Seq[Int] = card.ours.filter(card.winning.contains)

def day4(lines: List[String], part: Int = 1): Int =
    val pattern = "Card\\s+(\\d+):\\s+(.*) \\|\\s+(.*)".r

    val cards = lines.map {
        case pattern(id, winning, ours) => GamblingCard(
            id.toInt,
            winning.split("\\s+").map(_.toInt).toList,
            ours.split("\\s+").map(_.toInt).toList)
    }

    if (part == 1)
        cards
            .map(correct)
            .filter(_.nonEmpty)
            .map(nums => scala.math.pow(2, nums.size - 1).asInstanceOf[Int])
            .sum
    else
        var copies = cards
            .map(_ -> 1)
            .toMap

        for card <- cards do
            val numCopies = copies(card)
            val idsCopies = (card.id + 1) to (card.id + correct(card).size)
            for id <- idsCopies do
                copies.find(_._1.id == id) match {
                    case Some((key, value)) => copies = copies.updated(key, value + numCopies)
                    case None =>
                }

        copies.values.sum
