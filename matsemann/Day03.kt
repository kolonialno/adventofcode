package com.matsemann.adventofcode2024

import com.matsemann.adventofcode2024.utils.*

fun findMuls(line: String): Int =
    """mul\((\d+),(\d+)\)""".toRegex().findAll(line).sumOf {
        it.groupValues[1].toInt() * it.groupValues[2].toInt()
    }

fun day03_1(lines: List<String>): Any {
    return lines.sumOf { findMuls(it) }
}

fun day03_2(lines: List<String>): Any {
    return lines.joinToString().let { line ->
        line.split("do()").sumOf { part ->
            findMuls(part.split("don't()")[0])
        }
    }
}

fun main() {

//    run("1", fileName = "day03_ex.txt", func = ::day03_1)
    run("2", fileName = "day03_ex.txt", func = ::day03_2)


//    run("1", fileName = "day03.txt", func = ::day03_1)
    run("2", fileName = "day03.txt", func = ::day03_2)
}
