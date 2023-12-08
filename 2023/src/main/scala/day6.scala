private def getWaysToWin(time: Long, distance: Long): Int =
    (0L until time).count(sec => (sec * (time - sec)) > distance)

def day6(lines: List[String], part: Int = 1): Int =
    if part == 1 then
        val times = lines.head.split(":\\s+").last.split("\\s+").map(_.toLong).toList
        val distances = lines(1).split(":\\s+").last.split("\\s+").map(_.toLong).toList
        times
            .zip(distances)
            .map(getWaysToWin)
            .product
    else
        val time = lines.head.split(":\\s+").last.replaceAll("\\s+", "").toLong
        val distance = lines(1).split(":\\s+").last.replaceAll("\\s+", "").toLong
        getWaysToWin(time, distance)
