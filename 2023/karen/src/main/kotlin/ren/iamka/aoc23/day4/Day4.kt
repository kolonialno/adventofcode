package ren.iamka.aoc23.day4

import ren.iamka.aoc23.readLines
import kotlin.math.pow

fun main() {
    parsePart1()
}

private fun parsePart1() {
    return "/day4/data.txt".readLines {
        val points = map { line ->
            val (_, cards) = line.split(":")
            val (winningString, actualString) = cards.trim().split("|")
            val winning = winningString.getNumbers()
            val actual = actualString.getNumbers()

            val amountWinning = winning.intersect(actual)
            val points = if (amountWinning.isEmpty()) {
                0
            } else {
                val size = amountWinning.size.toDouble()
                2.0.pow(size - 1).toInt()
            }
            points
        }.sum()
        println(points)
    }
}

private fun String.getNumbers(): Set<Int> {
    return trim().split(" ").filter { it.isNotEmpty() }.map { it.toInt() }.toSet()
}