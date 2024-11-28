import scala.io.Source

val mapping = Map(
    "zero" -> 0,
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
)

def toDigit(s: String): Option[Int] =
    if s.isEmpty then
        None
    else if s(0).isDigit then
        Some(s(0) - '0')
    else
        mapping.find((k, v) => s.startsWith(k)).map(_._2)

def findDigits1(s: String) =
    s.toVector.filter(_.isDigit).map(_ - '0')

def findDigits2(s: String): IndexedSeq[Int] =
    s.scanRight("") { case (c, s) => c.toString + s}.flatMap(toDigit)

@main def calibration =
    val input = Source.fromFile("input.txt").getLines.toVector

    val part1 = input.map(findDigits1)
        .map(v => v.head * 10 + v.last).sum
    println("Part 1: " + part1)

    val part2 = input.map(findDigits2)
        .map(v => v.head * 10 + v.last).sum
    println("Part 2: " + part2)
