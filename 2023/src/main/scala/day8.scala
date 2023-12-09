private val pattern = "(\\w+) = \\((\\w+), (\\w+)\\)".r

extension [K, V](map: Map[K, V])
    def fetch(key: K): (K, V) = key -> map(key)

def day8(lines: List[String], part: Int = 1): Int =
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
        var node = nodes.fetch("AAA")
        var total = 0
        while node._1 != "ZZZ" do
            node = sequence(total % sequence.size) match {
                case 0 => nodes.fetch(node._2._1)
                case 1 => nodes.fetch(node._2._2)
            }
            total += 1

        total
    else
        var currentNodes = nodes.keySet.filter(_.endsWith("A")).map(nodes.fetch)
        var total = 0
        while !currentNodes.forall(_._1.endsWith("Z")) do
            currentNodes = currentNodes.map { node =>
                sequence(total % sequence.size) match {
                    case 0 => nodes.fetch(node._2._1)
                    case 1 => nodes.fetch(node._2._2)
                }
            }

            total += 1

        total
