import scala.io.Source

case class Range(start: Long, end: Long):
    def contains(l: Long) = l >= start && l <= end
    override def toString = s"$start to $end"

case class Mapping(range: Range, diff: Long)

case class Translation(from: String, to: String, mappings: Seq[Mapping]):
    def translate(pos: Long) =
        mappings.find(_.range contains pos).map(pos + _.diff) getOrElse pos

def parse(section: Seq[String]): Translation =
    val Array(from, _, to) = section.head.split(" ")(0).split("-")

    val mappings = for m <- section.tail
        Array(destStart, srcStart, length) = m.split(" ").map(_.toLong)
        range = Range(srcStart, srcStart + length - 1)
        diff = destStart - srcStart
    yield Mapping(range, diff)

    Translation(from, to, mappings.sortBy(_.range.start))


@main def seeds =
    val sections = Source.fromFile("input.txt").getLines.mkString("\n").split("\n\n").map(_.split("\n").toVector).toVector
    val seeds = sections(0)(0).split(": ")(1).split(" ").map(_.toLong).toVector

    val translations = sections.tail.map(parse)

    val distances = for seed <- seeds yield
        translations.foldLeft(seed)((pos, m) => m.translate(pos))

    println("Part 1: " + distances.min)
