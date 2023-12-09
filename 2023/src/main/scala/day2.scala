import cats.implicits._

private case class Game(id: Int, sets: Seq[Map[String, Int]])

private val pattern = "^Game (\\d+): (.*)$".r

def day2(lines: List[String], part: Int = 1): Int =
    val games = lines.map { line =>
        val pattern(gameId, setString) = line
        val sets = setString
            .split("; ")
            .map(_
                .split(", ")
                .map(_.split(" "))
                .map(item => item(1) -> item(0).toInt)
                .toMap)
        Game(gameId.toInt, sets)
    }

    if part == 1 then
        val goal = Map("red" -> 12, "green" -> 13, "blue" -> 14)
        games
            .filterNot(_.sets.exists(set => goal.exists((color, value) => set.getOrElse(color, 0) > value)))
            .map(_.id)
            .sum
    else
        games
            .map(_.sets
                .flatMap(_.toList)
                .groupMap(_._1)(_._2)
                .values
                .map(_.max)
                .product)
            .sum
