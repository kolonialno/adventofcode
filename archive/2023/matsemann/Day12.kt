package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*

fun day12_1(lines: List<String>): Any {

    return lines.map { line ->
        val (pattern, nums) = line.split(" ")
        pattern to nums.allInts()
    }.map { (pattern, numbers) ->
        val total = numbers.sum()
        val current = pattern.count { it == '#' }
        val missing = total - current

        val indexes = pattern.withIndex().filter { it.value == '?' }.map { it.index }

        indexes.combinations(length = missing).count { combination ->
            val arrangement = pattern.mapIndexed { i, c ->
                if (i in combination) {
                    '#'
                } else if (c == '?') {
                    '.'
                } else {
                    c
                }
            }.joinToString("") { it.toString() }

            arrangement.split(".").filter { it != "" }.map { it.length } == numbers
        }
    }.sum()
}


fun day12_2(lines: List<String>): Any {
    return "TODO"
}

fun main() {

    run("1", fileName = "day12_ex.txt", func = ::day12_1)
//    run("1", fileName = "day12_ex2.txt", func = ::day12_1)
//    run("2", fileName = "day12_ex.txt", func = ::day12_2)


    run("1", fileName = "day12.txt", func = ::day12_1)
//    run("2", fileName = "day12.txt", func = ::day12_2)
}
