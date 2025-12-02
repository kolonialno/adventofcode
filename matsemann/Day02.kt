package com.matsemann.adventofcode2025

import com.matsemann.adventofcode2025.utils.*
import kotlin.math.abs
import kotlin.math.max
import kotlin.math.min


fun day02_1(lines: List<String>): Any {
    return lines.first().split(",").map { r ->
        r.split("-").longs().let {
            it[0]..it[1]
        }
    }.flatMap { range ->
        range.filter {
            val str = it.toString()
            str.take(str.length / 2) == str.drop(str.length / 2)
        }
    }.sum()
}


fun day02_2(lines: List<String>): Any {
    return lines.first().split(",").map { r ->
        r.split("-").longs().let {
            it[0]..it[1]
        }
    }.flatMap { range ->
        range.filter {
            val str = it.toString()
            str.length > 1 && (1..((str.length + 1) / 2)).any { length ->
                str.chunked(length).all { it == str.take(length) }
            }
        }
    }.sum()
}

fun main() {

    run("1", fileName = "day02_ex.txt", func = ::day02_1)
//    run("2", fileName = "day02_ex.txt", func = ::day02_2)

    run("1", fileName = "day02.txt", func = ::day02_1)
//    run("2", fileName = "day02.txt", func = ::day02_2)
}
