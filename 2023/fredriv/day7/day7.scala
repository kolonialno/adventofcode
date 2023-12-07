import scala.io.Source

case class Card(c: Char):
    def strength = c match {
        case 'A' => 14
        case 'K' => 13
        case 'Q' => 12
        case 'J' => 11
        case 'T' => 10
        case c => c - '0'
    }


case class Hand(cards: Seq[Card], bid: Int) extends Ordered[Hand]:
    override def toString = s"Hand(${cards.map(_.c).mkString}, $bid)"

    def strength =
        val numOfAKind = cards.groupBy(_.c).view.mapValues(_.size).values.toSeq
        numOfAKind.max match {
            case 5 => 7 // Five of a kind
            case 4 => 6 // Four of a kind
            case 3 =>
                if numOfAKind.contains(2) then 5 // Full house
                else 4 // Three of a kind
            case 2 =>
                if numOfAKind.filter(_ == 2).size == 2 then 3 // Two pairs
                else 2 // One pair
            case 1 => 1 // High card
        }

    def compare(that: Hand) =
        if this.strength == that.strength then
            this.cards.zip(that.cards).map(_.strength - _.strength).filterNot(_ == 0).head
        else this.strength - that.strength


@main def cards =
    val input = Source.fromFile("input.txt").getLines.toVector

    val hands =
        for s <- input
            Array(cards, bid) = s.split(" ")
        yield Hand(cards.map(Card), bid.toInt)

    val ranked = hands.sorted.zipWithIndex
    val scores = ranked.map((hand, idx) => hand.bid * (idx + 1))
    println("Part 1: " + scores.sum)
