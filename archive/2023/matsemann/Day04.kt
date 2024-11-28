package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*
import kotlin.math.pow

fun day04_1(lines: List<String>): Any {
    return lines.map { line ->
        val parts = line.split(":", "|")
        val winning = parts[1].allInts().toSet()
        val my = parts[2].allInts().toSet()
        winning.intersect(my).size
    }.map {
        if (it == 0) {
            0
        } else {
            2.0.pow(it - 1).toInt()
        }
    }.sum()
}


fun day04_2(lines: List<String>): Any {
    val scratchcards = lines.associate { line ->
        val parts = line.split(":", "|")
        val winning = parts[1].allInts().toSet()
        val my = parts[2].allInts().toSet()
        val wins = winning.intersect(my).size
        parts[0].firstInt() to wins
    }

    val score = Cached<Int, Int> { card ->
        1 +
                (1..scratchcards[card]!!)
                    .sumOf { this(card + it) }
    }

    return scratchcards.map { score(it.key) }.sum()
}

fun main() {

//    run("1", fileName = "day04_ex.txt", func = ::day04_1)
    run("2", fileName = "day04_ex.txt", func = ::day04_2)


//    run("1", fileName = "day04.txt", func = ::day04_1)
    run("2", fileName = "day04.txt", func = ::day04_2)
}
