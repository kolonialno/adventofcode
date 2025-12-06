package com.matsemann.adventofcode2025

import com.matsemann.adventofcode2025.utils.*


fun day05_1(lines: List<String>): Any {
    return lines.splitBy { it == "" }.let {
        it[0].map { r -> r.split("-").longs().let { n -> n[0]..n[1] } } to
                it[1].longs()

    }.let { (ranges, ingredients) ->
        ingredients.count { i -> ranges.any { r -> i in r } }
    }

}


fun day05_2(lines: List<String>): Any {
    val ranges = lines.splitBy { it == "" }.let {
        it[0].map { r -> r.split("-").longs().let { n -> n[0]..n[1] } }
    }

    var i = 0
    var r = ranges
    while (i < r.size) {
        val current = r[i]
        val (overlapping, remaining) = r.drop(i + 1).partition { it.overlaps(current) }
        if (overlapping.isNotEmpty()) {
            val merged = overlapping.fold(current) { acc, next -> acc.merge(next) }
            r = r.take(i) + remaining + listOf(merged)
        } else {
            i++
        }
    }
    return r.sumOf { range ->
        range.last - range.first + 1
    }
}


fun main() {

//    run("1", fileName = "day05_ex.txt", func = ::day05_1)
    run("2", fileName = "day05_ex.txt", func = ::day05_2)


//    run("1", fileName = "day05.txt", func = ::day05_1)
    run("2", fileName = "day05.txt", func = ::day05_2)
}
