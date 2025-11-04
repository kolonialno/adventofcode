import scala.io.Source
import scala.compiletime.ops.double

@main def day04 =
    val input = Source.fromFile("input.txt").getLines.map(_.toArray).toArray

    val horizontal = input.map(_.mkString)
    val vertical = input.transpose.map(_.mkString)
    val diagonal = diagonals(input) ++ diagonals(input.map(_.reverse))
    val total = (horizontal ++ vertical ++ diagonal).map(line =>
        line.sliding(4).count(s => s == "XMAS" || s == "SAMX")
    ).sum

    println("Part 1: " + total)
    println("Part 2: " + findXMas(input))


def findXMas(a: Array[Array[Char]]): Int =
    var total = 0
    for x <- 1 until a.length - 1
        y <- 1 until a.length - 1
    do
        if a(y)(x) == 'A' &&
            Set(a(y-1)(x-1), a(y+1)(x+1)) == Set('M', 'S') &&
            Set(a(y-1)(x+1), a(y+1)(x-1)) == Set('M', 'S')
        then total += 1

    total


def diagonals(a: Array[Array[Char]]): IndexedSeq[String] =
    val h = a.length
    val w = a(0).length
    for d <- 0 until (h + w - 1) yield
        val sb = new StringBuilder
        for x <- 0 to d do
            val y = d - x
            if y < h && x < w then
                sb.append(a(y)(x))
        sb.toString
