package com.matsemann.adventofcode2024

import com.matsemann.adventofcode2024.utils.*
import kotlin.math.abs


fun day01_1(lines: List<String>): Any {
    return lines.map {
        it.allInts()
    }.transpose().let {
        it[0].sorted().zip(it[1].sorted())
    }.sumOf {
        abs(it.second - it.first)
    }
}


fun day01_2(lines: List<String>): Any {
    return lines.map {
        it.allInts()
    }.transpose().let { (a, b) ->
        a.sumOf { v ->
            b.count { v == it } * v
        }
    }
}

fun main() {
    run("1", fileName = "day01_ex.txt", func = ::day01_1)
    run("2", fileName = "day01_ex.txt", func = ::day01_2)

    run("1", fileName = "day01.txt", func = ::day01_1)
    run("2", fileName = "day01.txt", func = ::day01_2)
}
