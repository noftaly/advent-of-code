private val pattern = "(\\d+) (\\d+) (\\d+)".r

private case class Mapping(start: Long, end: Long, offset: Long)

private def binarySearchMapping(mapping: List[Mapping], value: Long): Option[Int] =
    var end = mapping.size - 1
    var start = 0

    while start <= end do
        val middle = (start + end) / 2
        val map = mapping(middle)
        if value >= map.start && value <= map.end then
            return Some(middle)
        else if value > map.end then
            start = middle + 1
        else
            end = middle - 1

    None

def day5(lines: List[String], part: Int = 1): Long =
    val seeds = lines.head.substring(7).split(" ").map(_.toLong)
    var sections = Array[List[Mapping]]()

    val start = System.nanoTime()

    for line <- lines.tail.filterNot(_.isBlank) do
        if line.endsWith("map:") then
            sections :+= List()
        else
            line match {
                case pattern(destinationStartStr, sourceStartStr, lengthStr) =>
                    val start = sourceStartStr.toLong
                    val end = start + lengthStr.toLong - 1
                    val offset = destinationStartStr.toLong - start
                    sections.update(sections.length - 1, sections.last :+ Mapping(start, end, offset))
            }

    sections = sections.map(_.sortBy(_.start))

    def getDest(value: Long): Long =
        sections
            .foldLeft(value) { (current, section) =>
                binarySearchMapping(section, current) match {
                    case Some(idx) => current + section(idx).offset
                    case None => current
                }
            }

    // Ended up doing the exact same algorithm, but in python. The scala one would run out of memory very fast.
    if part == 1 then
        seeds.map(getDest).min
    else
        val values = for
            Array(start, offset) <- seeds.grouped(2)
            i <- start until (start + offset)
        yield i

        val result = values.map(getDest).min

        println(s"Time (s): ${(System.nanoTime() - start) / 1000000000}")

        result
