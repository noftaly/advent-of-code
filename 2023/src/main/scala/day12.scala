import scala.collection.mutable

def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {self =>
    override def apply(key: I) = self.synchronized(getOrElseUpdate(key, f(key)))
}

private def analyze(record: String, groups: Seq[Int]): Int =
    if groups.isEmpty then // If there are no groups anymore, check that there are no springs anymore either
        if !record.contains("#") then return 1
        else return 0

    if record.isEmpty then return 0 // If we finished exploring our line but there are more groups, then it is not viable

    def pound(): Int =
        // If we arrived at a spring, then it must conform to the next group, that means that we must have enough
        // springs left to make a group
        val nextGroup = groups.head
        val thisGroup = record.take(nextGroup).replace("?", "#")

        if thisGroup != "#" * nextGroup then // If the group isn't valid, then it doesn't work
            0
        else if record.length == nextGroup then // If we are at the end of our record, make sur we are done with the groups
            if groups.size == 1 then 1
            else 0
        else if record(nextGroup) == '#' then // If the character after is a spring, then we messed up
            0
        else // Otherwise, it is valid and there are more groups to go, so we analyze the rest
            analyze(record.substring(nextGroup + 1), groups.tail)

    def dot(): Int =
        analyze(record.substring(1), groups) // Ignore and go to the next character

    record(0) match {
        case '#' => pound()
        case '.' => dot()
        case '?' => dot() + pound()
        case _ => throw new RuntimeException
    }

private val pattern = "(.*)\\s(.*)".r

def day12(lines: List[String], part: Int = 1): Int =
    val mapper = memoize(analyze.tupled)

    val springRows = lines
        .map {
            case pattern(plan, groups) => (plan, groups.split(",").map(_.toInt).toSeq)
        }

    if part == 1 then
        springRows.map(mapper).sum
    else
        // Ended up doing the exact same algorithm, but in python. The scala one would stall at line 19.
        // Probably messed up the memoization
        springRows
            .map((record, groups) => (List.fill(5)(record).mkString("?"), List.fill(5)(groups).flatten))
            .map(mapper)
            .sum
