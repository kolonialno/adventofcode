package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*

fun day01_1(lines: List<String>): Any {
    return lines
        .map { it.split("").ints() }
        .map { it.first().toString() + it.last().toString() }
        .map { it.toInt() }
        .sum()
}


fun day01_2(lines: List<String>): Any {
    val replacements = listOf(
        "one" to "1",
        "two" to "2",
        "three" to "3",
        "four" to "4",
        "five" to "5",
        "six" to "6",
        "seven" to "7",
        "eight" to "8",
        "nine" to "9"
    )

    return lines
        .map {
            // substitute => "one" to "one1one" etc (to survive eightwo otherwise being eigh2 after a replacement)
            replacements.fold(it) { acc, pair ->
                acc.replace(pair.first, pair.first + pair.second + pair.first)
            }
        }
        // Then just use logic from part1
        .map { it.split("").ints() }
        .map { it.first().toString() + it.last().toString() }
        .map { it.toInt() }
        .sum()
}

fun main() {

//    run("1", fileName = "day01_ex.txt", func = ::day01_1)
    run("2", fileName = "day01_ex2.txt", func = ::day01_2)


//    run("1", fileName = "day01.txt", func = ::day01_1)
    run("2", fileName = "day01.txt", func = ::day01_2)
}
