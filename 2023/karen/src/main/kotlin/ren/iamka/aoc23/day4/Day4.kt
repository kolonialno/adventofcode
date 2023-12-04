package ren.iamka.aoc23.day4

import ren.iamka.aoc23.readLines
import kotlin.math.pow

data class ScratchCard(val id: Int, val winningNumbers: Int)

fun main() {
    part1()
    part2()
}

private fun parseScratchCards(operation: Sequence<ScratchCard>.() -> Unit) {
    return "/day4/data.txt".readLines {
        map { line ->
            val (cardIdString, cards) = line.split(":")
            val (_, idString) = cardIdString.split("Card ")
            val cardId = idString.trim().toInt()
            val (winningString, actualString) = cards.trim().split("|")
            val winning = winningString.getNumbers()
            val actual = actualString.getNumbers()
            val winningNumbers = winning.intersect(actual)
            ScratchCard(id = cardId, winningNumbers = winningNumbers.size)
        }.operation()
    }
}


private fun part1() {
    parseScratchCards {
        val points = this.map { card ->
            val points = if (card.winningNumbers == 0) {
                0
            } else {
                val size = card.winningNumbers.toDouble()
                2.0.pow(size - 1).toInt()
            }
            points
        }.sum()
        println(points)
    }
}

private fun part2() {
    parseScratchCards {
        val cardToAmount = mutableMapOf<Int, Int>()
        forEach { card ->
            val currentAmount = cardToAmount.getOrPut(card.id) { 1 }
            repeat(currentAmount) {
                (card.id + 1..card.id + card.winningNumbers).forEach { id ->
                    cardToAmount.compute(id) { _, value ->
                        if (value == null) {
                            2
                        } else {
                            value + 1
                        }
                    }
                }

            }
        }
        val numberOfCards = cardToAmount.values.sum()
        println(numberOfCards)
    }
}

private fun String.getNumbers(): Set<Int> {
    return trim().split(" ").filter { it.isNotEmpty() }.map { it.toInt() }.toSet()
}

