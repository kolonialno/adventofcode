import scala.io.Source

case class Pos(x: Int, y: Int)

case class Number(number: Int, pos: Pos):
    val length = number.toString.length

case class Symbol(symbol: Char, pos: Pos)

def parse(lines: Seq[String]) =
    val numbers =
        for (line, y) <- lines.zipWithIndex
            m <- "\\d+".r.findAllMatchIn(line)
        yield Number(m.matched.toInt, Pos(m.start, y))

    val symbols =
        for (line, y) <- lines.zipWithIndex
            m <- "[^\\.0-9]".r.findAllMatchIn(line)
        yield Symbol(m.matched(0), Pos(m.start, y))

    (numbers, symbols)

def isAdjacent(n: Number, s: Symbol): Boolean =
    s.pos.x >= n.pos.x - 1 && s.pos.x <= n.pos.x + n.length &&
    s.pos.y >= n.pos.y - 1 && s.pos.y <= n.pos.y + 1

def findAdjacentSymbol(number: Number, symbols: Seq[Symbol]): Option[Symbol] =
    symbols.find(symbol => isAdjacent(number, symbol))

def findAdjacentNumbers(symbol: Symbol, numbers: Seq[Number]): Seq[Number] =
    numbers.filter(number => isAdjacent(number, symbol))

@main def partnumbers =
    val input = Source.fromFile("input.txt").getLines.toVector
    val (numbers, symbols) = parse(input)

    val filtered = numbers.filter(number => findAdjacentSymbol(number, symbols).nonEmpty)
    println("Part 1: " + filtered.map(_.number).sum)

    val gearratios = symbols
        .map(s => findAdjacentNumbers(s, numbers))
        .filter(_.length == 2)
        .map(nums => nums(0).number * nums(1).number)

    println("Part 2: " + gearratios.sum)
