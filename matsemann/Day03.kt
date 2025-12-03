package com.matsemann.adventofcode2025

import com.matsemann.adventofcode2025.utils.*


fun day03_1(lines: List<String>): Any {
    return lines.map { line ->
        line.split("").ints().let { bank ->
            val first = bank.dropLast(1).withIndex().maxBy { it.value }
            val second = bank.drop(first.index + 1).max()
            first.value * 10 + second
        }
    }.sum()
}


fun day03_2(lines: List<String>): Any {
    return lines.map { line ->
        line.split("").ints().let { bank ->
            (11 downTo 0).fold(0L to 0) { (total, prevIndex), pos ->
                val found = bank.drop(prevIndex).dropLast(pos).withIndex().maxBy { it.value }
                total * 10 + found.value to found.index + 1 + prevIndex
            }
        }
    }.sumOf { it.first }
}

fun main() {

//    run("1", fileName = "day03_ex.txt", func = ::day03_1)
    run("2", fileName = "day03_ex.txt", func = ::day03_2)


//    run("1", fileName = "day03.txt", func = ::day03_1)
    run("2", fileName = "day03.txt", func = ::day03_2)
}
