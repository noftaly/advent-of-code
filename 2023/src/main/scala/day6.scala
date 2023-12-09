private def getWaysToWin(time: Long, distance: Long): Int =
    (0L until time).count(sec => (sec * (time - sec)) > distance)

def day6(lines: List[String], part: Int = 1): Int =
    val parsed = lines.map(_.split(":\\s+").last)

    if part == 1 then
        val times = parsed.head.split("\\s+").map(_.toLong).toList
        val distances = parsed.last.split("\\s+").map(_.toLong).toList
        times
            .zip(distances)
            .map(getWaysToWin)
            .product
    else
        val time = parsed.head.replaceAll("\\s+", "").toLong
        val distance = parsed.last.replaceAll("\\s+", "").toLong
        getWaysToWin(time, distance)
