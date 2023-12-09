private def getWaysToWin(time: BigInt, distance: BigInt): Int =
    (BigInt(0) until time).count(sec => (sec * (time - sec)) > distance)

def day6(lines: List[String], part: Int = 1): Int =
    val parsed = lines.map(_.split(":\\s+").last)

    if part == 1 then
        val times = parsed.head.split("\\s+").map(BigInt(_)).toList
        val distances = parsed.last.split("\\s+").map(BigInt(_)).toList
        times
            .zip(distances)
            .map(getWaysToWin)
            .product
    else
        val time = parsed.head.replaceAll("\\s+", "")
        val distance = parsed.last.replaceAll("\\s+", "")
        getWaysToWin(BigInt(time), BigInt(distance))
