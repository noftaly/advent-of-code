import scala.collection.mutable

case class Lens(label: String, focal: Int)

def hash(word: String): Int = word.foldLeft(0) { (acc, char) => ((acc + char.toInt) * 17) % 256 }

def day15(lines: List[String], part: Int = 1): BigInt =
    val instructions = lines.mkString(",").split(",")

    if part == 1 then
        instructions.map(hash).sum
    else
        val boxes = (0 to 255).map(_ -> mutable.Queue[Lens]()).toMap

        instructions
            .foreach(instr =>
                val label = instr.split("(-|=)").head
                val slots = boxes(hash(label))

                if instr.endsWith("-") then
                    slots.dequeueFirst(_.label == label)
                else
                    val focalLength = instr.split("=").last.toInt
                    val lens = Lens(label, focalLength)
                    if slots.exists(_.label == label) then
                        slots.update(slots.indexWhere(_.label == label), lens)
                    else
                        slots.enqueue(lens)
            )

        boxes
            .flatMap((box, lenses) => lenses.zipWithIndex.map((lens, idx) => (box, lens, idx)))
            .map((box, lens, idx) => (1 + box) * (1 + idx) * lens.focal)
            .sum
