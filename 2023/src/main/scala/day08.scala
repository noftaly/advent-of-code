private val pattern = "(\\w+) = \\((\\w+), (\\w+)\\)".r

def countSteps(nodes: Map[String, (String, String)], sequence: List[Int], start: String, goal: String): Int =
    var node = start
    var steps = 0
    while !node.endsWith(goal) do
        node = sequence(steps % sequence.size) match {
            case 0 => nodes(node)(0)
            case 1 => nodes(node)(1)
        }
        steps += 1
    steps

def day8(lines: List[String], part: Int = 1): BigInt =
    val sequence = lines.head.map {
        case 'L' => 0
        case 'R' => 1
        case char => throw new IllegalArgumentException(s"Unknown sequence char $char")
    }.toList
    // Skip the first and second lines
    val nodes = lines.tail.tail.map {
        case pattern(name, left, right) => name -> (left, right)
    }.toMap

    if part == 1 then
        countSteps(nodes, sequence, "AAA", "ZZZ")
    else
        // With the help of internet to know I had to use the LCM
        val steps = nodes
            .keySet
            .filter(_.endsWith("A"))
            .map(node => countSteps(nodes, sequence, node, "Z"))
            .map(BigInt(_))
            .toSeq
        lcm(steps)
