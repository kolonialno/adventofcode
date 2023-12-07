package ren.iamka.aoc23.day7

import ren.iamka.aoc23.readLines

fun main() {
    parse(toHand = {it.cardsToHand()}){
        it.calculateTotalWinningsPart1()
    }

    parse(toHand = {it.cardsToHandWithJoker()}){
        it.calculateTotalWinningsPart2()
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


fun Map<Hand, Int>.calculateTotalWinningsPart1() {
    val sorted = this.toSortedMap(HandComparator())

    sorted.entries.mapIndexed { i, (_, bid) ->
        (i + 1) * bid
    }.sum().apply { println("part 1: $this") }
}

fun Map<Hand, Int>.calculateTotalWinningsPart2() {
    val sorted = this.toSortedMap(HandComparator(CardComparator(validCardsPart2)))

    sorted.entries.mapIndexed { i, (_, bid) ->
        (i + 1) * bid
    }.sum().apply { println("part 2: $this") }
}