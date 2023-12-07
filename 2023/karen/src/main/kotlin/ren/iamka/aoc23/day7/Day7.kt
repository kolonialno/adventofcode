package ren.iamka.aoc23.day7

import ren.iamka.aoc23.readLines

fun main() {
    parse()
}

fun parse() {
    "/day7/data.txt".readLines {
        val lines = this.toList()
        val cardsToBid = lines.associate {
            val (cards, bidString) = it.split(' ')
            cards.trim().cardsToHand() to bidString.trim().toInt()
        }

        cardsToBid.calculateTotalWinnings()
    }
}


fun Map<Hand, Int>.calculateTotalWinnings() {
    val sorted = this.toSortedMap(HandComparator())

    sorted.entries.mapIndexed { i, (_, bid) ->
        (i + 1) * bid
    }.sum().apply { println("part 1: $this") }
}