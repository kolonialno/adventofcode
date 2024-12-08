import scala.io.Source

@main def day03 =
    val input = Source.fromFile("input.txt").getLines.mkString("\n")

    val mulRegex = """mul\((\d+),(\d+)\)""".r
    val products = for m <- mulRegex.findAllMatchIn(input) yield
        m.group(1).toInt * m.group(2).toInt
    println("Part 1: " + products.sum)

    val doDontMulRegex = """mul\((\d+),(\d+)\)|do\(\)|don't\(\)""".r
    var enabled = true
    var total = 0
    for m <- doDontMulRegex.findAllMatchIn(input) do
        if m.matched == "do()" then enabled = true
        else if m.matched == "don't()" then enabled = false
        else if enabled then total += m.group(1).toInt * m.group(2).toInt
    println("Part 2: " + total)
