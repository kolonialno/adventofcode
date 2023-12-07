package ren.iamka.aoc23.day7

import ren.iamka.aoc23.readLines

fun main() {
    parse(toHand = {it.cardsToHand()}){
        it.calculateTotalWinnings(HandComparator())
    }

    parse(toHand = {it.cardsToHandWithJoker()}){
        it.calculateTotalWinnings(HandComparator(CardComparator(validCardsPart2)))
    }
}

fun parse(toHand: (String) -> Hand, operation: (Map<Hand, Int>) -> Unit) {
    "/day7/data.txt".readLines {
        val lines = this.toList()
        val cardsToBid = lines.associate {
            val (cards, bidString) = it.split(' ')
            toHand(cards.trim()) to bidString.trim().toInt()
        }
        operation(cardsToBid)
    }
}


fun Map<Hand, Int>.calculateTotalWinnings(handComparator: HandComparator) {
    val sorted = this.toSortedMap(handComparator)
    sorted.entries.mapIndexed { i, (_, bid) ->
        (i + 1) * bid
    }.sum().apply { println(this) }
}