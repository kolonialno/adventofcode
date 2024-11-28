import scala.io.Source

case class Reveal(r: Int, g: Int, b: Int):
    def >(that: Reveal) = r > that.r || g > that.g || b > that.b
    def max(that: Reveal) = Reveal(Math.max(r, that.r), Math.max(g, that.g), Math.max(b, that.b))

case class Game(id: Int, reveals: IndexedSeq[Reveal])

def parseRound(r: String): Reveal =
    r.split(", ").foldLeft(Reveal(0, 0, 0)) { case (cur, colStr) =>
        val Array(n, color) = colStr.split(" ")
        color match
            case "red" => cur.copy(r = cur.r + n.toInt)
            case "green" => cur.copy(g = cur.g + n.toInt)
            case "blue" => cur.copy(b = cur.b + n.toInt)
    }

def parseGame(s: String): Game =
    val Array(gameStr, roundsStr) = s.split(": ")
    val gameId = gameStr.split(" ")(1).toInt
    val reveals = roundsStr.split("; ").map(parseRound)
    Game(gameId, reveals)

def maxRevealed(game: Game): Reveal =
    game.reveals.reduce(_ max _)

@main def idsums =
    val input = Source.fromFile("input.txt").getLines.toVector

    val limit = Reveal(12, 13, 14)
    val part1 = input.map(parseGame).filterNot(g => maxRevealed(g) > limit).map(_.id).sum
    println(s"Part 1: $part1")

    val part2 = input.map(parseGame).map(maxRevealed).map(r => r.r * r.g * r.b).sum
    println(s"Part 2: $part2")