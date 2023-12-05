import scala.io.Source

case class Range(start: Long, end: Long):
    def contains(l: Long) = l >= start && l <= end
    def size = end - start + 1

    def intersect(that: Range): Option[Range] =
        if that.end < start || end < that.start then
            None
        else
            Some(Range(Math.max(start, that.start), Math.min(end, that.end)))

    override def toString = s"$start to $end"

def mergeRanges(ranges: Vector[Range]): Vector[Range] =
    if ranges.length <= 1 then ranges
    else
        var result = Vector(ranges.head)
        for range <- ranges.tail do
            if result.last.end < range.start - 1 then result :+= range
            else result = result.take(result.length - 1) :+ Range(result.last.start, Math.max(result.last.end, range.end))
        result

case class Mapping(range: Range, diff: Long)

case class Translation(from: String, to: String, mappings: Seq[Mapping]):
    def translate(pos: Long) =
        mappings.find(_.range contains pos).map(pos + _.diff) getOrElse pos
    def translate(range: Range): Seq[Range] =
        var result = Vector[Range]()
        var remainder = Vector(range)
        for m <- mappings do
            var newRemainder = Vector[Range]()
            for r <- remainder do
                val before = if r.start >= m.range.start then None else Some(Range(r.start, Math.min(r.end, m.range.start - 1)))
                val intersect = r.intersect(m.range)
                val after = if r.end <= m.range.end then None else Some(Range(Math.max(r.start, m.range.end + 1), r.end))
                result ++= intersect.map(r => Range(r.start + m.diff, r.end + m.diff)).toVector
                newRemainder ++= (before.toVector ++ after.toVector)
            remainder = mergeRanges(newRemainder.sortBy(_.start))
        result ++ remainder

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

    var ranges = seeds.sliding(2, 2).toVector.map(v => Range(v(0), v(0) + v(1))).sortBy(r => r.start)
    for t <- translations do
        val newRanges = ranges.flatMap(t.translate).sortBy(_.start)
        ranges = mergeRanges(newRanges)

    println("Part 2: " + ranges.head.start)
