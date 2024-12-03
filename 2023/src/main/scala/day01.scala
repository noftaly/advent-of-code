def spelledOutToInt(str: String): Int =
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
        .toInt

def day1(lines: List[String], part: Int = 1): Int =
    val constNumbers: Seq[String] =
        if (part == 1) Seq("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
        else Seq("zero", "0", "one", "1", "two", "2", "three", "3", "four", "4", "five", "5", "six", "6", "seven", "7", "eight", "8", "nine", "9")

    lines
        .map(line => (findFirstAmong(line, constNumbers), findLastAmong(line, constNumbers)))
        .map((first, last) => spelledOutToInt(first) * 10 + spelledOutToInt(last))
        .sum
