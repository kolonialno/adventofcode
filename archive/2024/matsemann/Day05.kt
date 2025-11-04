package com.matsemann.adventofcode2024

import com.matsemann.adventofcode2024.utils.*


fun comparator(rules: Set<String>) = Comparator { value: String, secondValue: String ->
    if (rules.contains("$value|$secondValue")) {
        -1
    } else if (rules.contains("$secondValue|$value")) {
        1
    } else {
        0
    }
}
fun day05_1(lines: List<String>): Any {
    val (ruleLines, prints) = lines.splitBy { it == "" }
    val compare = comparator(ruleLines.toSet())

    return prints.map {
        it.split(",")
    }.map {
        it.sortedWith(compare) to it
    }.filter { (sorted, original) ->
        sorted == original
    }.sumOf { (_, original) ->
        original[original.size / 2].toInt()
    }
}

fun day05_2(lines: List<String>): Any {
    val (ruleLines, prints) = lines.splitBy { it == "" }
    val compare = comparator(ruleLines.toSet())

    return prints.map {
        it.split(",")
    }.map {
        it.sortedWith(compare) to it
    }.filter { (sorted, original) ->
        sorted != original
    }.sumOf { (sorted) ->
        sorted[sorted.size / 2].toInt()
    }
}

fun main() {

//    run("1", fileName = "day05_ex.txt", func = ::day05_1)
//    run("2", fileName = "day05_ex.txt", func = ::day05_2)


    run("1", fileName = "day05.txt", func = ::day05_1)
    run("2", fileName = "day05.txt", func = ::day05_2)
}
