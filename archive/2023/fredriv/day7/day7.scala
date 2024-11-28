import scala.io.Source

case class Card(c: Char, jokers: Boolean = false):
    def strength = c match {
        case 'A' => 14
        case 'K' => 13
        case 'Q' => 12
        case 'J' => if jokers then 1 else 11
        case 'T' => 10
        case c => c - '0'
    }


case class Hand(cards: Seq[Card], bid: Int, jokers: Boolean = false) extends Ordered[Hand]:
    override def toString = s"Hand(${cards.map(_.c).mkString}, $bid${if jokers then ", jokers" else ""})"

    lazy val strength =
        val numJokers = if jokers then cards.count(_.c == 'J') else 0
        val numOfAKind = cards
            .filter(card => !jokers || card.c != 'J')
            .groupBy(_.c)
            .mapValues(_.size)
            .values.toSeq

        val max = if numOfAKind.isEmpty then numJokers else numOfAKind.max + numJokers
        val numPairs = numOfAKind.count(_ == 2)
        max match {
            case 5 => 7 // Five of a kind
            case 4 => 6 // Four of a kind
            case 3 =>
                if numJokers == 1 && numPairs == 2 then 5 // Full house
                else if numJokers == 0 && numPairs == 1 then 5 // Full house
                else 4 // Three of a kind
            case 2 =>
                if numPairs == 2 then 3 // Two pairs
                else 2 // One pair
            case 1 => 1 // High card
        }

    def compare(that: Hand) =
        if this.strength == that.strength then
            this.cards.zip(that.cards).map(_.strength - _.strength).find(_ != 0).get
        else this.strength - that.strength


@main def cards =
    val input = Source.fromFile("input.txt").getLines.toVector

    val hands =
        for s <- input
            Array(cards, bid) = s.split(" ")
        yield Hand(cards.map(Card(_)), bid.toInt)

    val ranked = hands.sorted.zipWithIndex
    val scores = ranked.map((hand, idx) => hand.bid * (idx + 1))
    println("Part 1: " + scores.sum)

    val handsWithJoker =
        for s <- input
            Array(cards, bid) = s.split(" ")
        yield Hand(cards.map(Card(_, jokers = true)), bid.toInt, jokers = true)

    val rankedWithJoker = handsWithJoker.sorted.zipWithIndex
    val scoresWithJoker = rankedWithJoker.map((hand, idx) => (idx + 1, hand, hand.strength, hand.bid.toLong * (idx + 1)))
    println("Part 2: " + scoresWithJoker.map(_._4).sum)
