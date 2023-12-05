import scala.io.Source
import collection.mutable.ArrayBuffer

case class Card(id: Int, numbers: Set[Int], winning: Set[Int]):
    val wins = numbers.intersect(winning).size
    val score = if wins == 0 then 0 else Math.pow(2, wins - 1).toInt

def findNums(s: String) = "\\d+".r.findAllIn(s).map(_.toInt).toSet

def parse(lines: Seq[String]): Seq[Card] =
    val pattern = raw"Card\s+(\d+): (.*) \| (.*)".r

    for line <- lines
        m <- pattern.findFirstMatchIn(line)
        List(cardId, wins, nums) = m.subgroups
    yield
        Card(cardId.toInt, findNums(nums), findNums(wins))

@main def scratchcards =
    val input = Source.fromFile("input.txt").getLines.toVector
    val cards = parse(input)

    println(s"Part 1: " + cards.map(_.score).sum)

    val numCards = ArrayBuffer.fill(cards.length)(1)
    for (c, i) <- cards.zipWithIndex
        j <- 1 to c.wins
        if i + j < numCards.size
    do
        numCards(i+j) += numCards(i)

    println(s"Part 2: " + numCards.sum)
