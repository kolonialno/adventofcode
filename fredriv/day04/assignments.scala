import scala.io.Source

def contained(ranges: (Range, Range)): Boolean = ranges match
  case (r1, r2) => r1.intersect(r2) == r1 || r2.intersect(r1) == r2

def overlaps(ranges: (Range, Range)): Boolean = ranges match
  case (r1, r2) => r1.intersect(r2).nonEmpty

def toRanges(assignment: String): (Range, Range) =
  val Array(r1, r2) =
    for r <- assignment.split(",")
        Array(from, to) = r.split("-").map(_.toInt)
    yield from.to(to)
  (r1, r2)

@main def main =
  val assignments = Source.fromFile("input.txt").getLines.map(toRanges).toList

  println("Part1: " + assignments.filter(contained).length)
  println("Part2: " + assignments.filter(overlaps).length)
