private def findFirstAmong(str: String, occurrences: Seq[String]): String =
    var min = Int.MaxValue
    var minValue = ""

    for word <- occurrences do
        val a = str.indexOf(word)
        if a != -1 && a < min then
            min = a
            minValue = word

    minValue

private def findLastAmong(str: String, occurrences: Seq[String]): String =
    var max = Int.MinValue
    var maxValue = ""

    for word <- occurrences do
        val a = str.lastIndexOf(word)
        if a != -1 && a > max then
            max = a
            maxValue = word

    maxValue

private def spelledOutToInt(str: String): String =
    str.replace("zero", "0")
        .replace("one", "1")
        .replace("two", "2")
        .replace("three", "3")
        .replace("four", "4")
        .replace("five", "5")
        .replace("six", "6")
        .replace("seven", "7")
        .replace("eight", "8")
        .replace("nine", "9")

def day1(lines: List[String], part: Int = 1): Int =
    var numbers = List[Int]()

    val constNumbers: Seq[String] =
        if (part == 1) Seq("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
        else Seq("zero", "0", "one", "1", "two", "2", "three", "3", "four", "4", "five", "5", "six", "6", "seven", "7", "eight", "8", "nine", "9")

    for line <- lines do
        val first = findFirstAmong(line, constNumbers)
        val last = findLastAmong(line, constNumbers)
        numbers :+=  s"${spelledOutToInt(first)}${spelledOutToInt(last)}".toInt

    numbers.sum

