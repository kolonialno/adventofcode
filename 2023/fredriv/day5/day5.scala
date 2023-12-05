import scala.io.Source

case class Range(start: Long, end: Long):
    def contains(l: Long) = l >= start && l <= end
    override def toString = s"$start to $end"

case class Mapping(range: Range, diff: Long)

case class Mappings(from: String, to: String, mappings: Seq[Mapping]):
    def translate(pos: Long) =
        val result = mappings.find(m => m.range.contains(pos)).map(m => pos + m.diff).getOrElse(pos)
        result

def parse(sections: Seq[Seq[String]]): Seq[Mappings] =
    for section <- sections
        Array(from, _, to) = section.head.split(" ")(0).split("-")

        mappings = for m <- section.tail
            Array(destStart, srcStart, length) = m.split(" ").map(_.toLong)
            range = Range(srcStart, srcStart + length - 1)
            diff = destStart - srcStart
        yield Mapping(range, diff)

    yield Mappings(from, to, mappings.sortBy(m => m.range.start))


@main def seeds =
    val sections = Source.fromFile("input.txt").getLines.mkString("\n").split("\n\n").map(_.split("\n").toVector).toVector
    val seeds = sections(0)(0).split(": ")(1).split(" ").map(_.toLong).toVector

    val mappings = parse(sections.tail)

    val distances = for seed <- seeds yield
        mappings.foldLeft(seed)((pos, m) => m.translate(pos))

    println("Part 1: " + distances.min)
