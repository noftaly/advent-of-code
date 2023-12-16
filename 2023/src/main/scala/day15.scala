case class Lens(label: String, focal: Int)

def hash(word: String): Int = word.foldLeft(0) { (acc, char) => ((acc + char.toInt) * 17) % 256 }

def day15(lines: List[String], part: Int = 1): BigInt =
    val instructions = lines.mkString(",").split(",")

    if part == 1 then
        instructions.map(hash).sum
    else
        val boxes = Vector.fill(256)(Vector.empty[Lens])

        instructions
            .foldLeft(boxes)((boxes, instr) =>
                val label = instr.split("(-|=)").head
                val computedHash = hash(label)
                val box = boxes(computedHash)

                if instr.endsWith("-") then
                    boxes.updated(computedHash, box.filterNot(_.label == label))
                else
                    val focalLength = instr.split("=").last.toInt
                    val lens = Lens(label, focalLength)
                    box.indexWhere(_.label == label) match {
                        case -1 => boxes.updated(computedHash, box.appended(lens))
                        case i => boxes.updated(computedHash, box.updated(i, lens))
                    }
            )
            .zipWithIndex
            .map((lenses, boxIndex) =>
                lenses.zipWithIndex.map((lens, lensIndex) => (1 + boxIndex) * (1 + lensIndex) * lens.focal).sum)
            .sum
