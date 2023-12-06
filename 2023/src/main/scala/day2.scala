import cats.implicits._

private case class Game(id: Int, sets: Seq[Map[String, Int]])

private val pattern = "^Game (\\d+): (.*)$".r

def day2(lines: List[String], part: Int = 1): Int =
    var games = List[Game]()
    val goal = Map(
        "red" -> 12,
        "green" -> 13,
        "blue" -> 14,
    )

    for line <- lines do
        val pattern(gameId, setString) = line

        val sets = setString.split("; ")
            .map(_
                .split(", ")
                .map(_.split(" "))
                .map(item => item(1) -> item(0).toInt)
                .toMap)

        games :+= Game(gameId.toInt, sets)

    if part == 1 then
        games
            .filterNot(_.sets.exists { set =>
                goal.exists(kv => set.getOrElse(kv._1, 0) > kv._2)
            }
            )
            .map(_.id)
            .sum
    else
        games.map(_.sets
                .flatMap(_.toList)
                .groupMap(_._1)(_._2)
                .view
                .mapValues(_.max)
                .toMap)
            .map(kv => kv.values.product)
            .sum
