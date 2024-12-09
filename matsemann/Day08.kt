package com.matsemann.adventofcode2024

import com.matsemann.adventofcode2024.utils.*
import com.matsemann.adventofcode2024.utils.IntVec.Companion.allWithinBounds


fun day08_1(lines: List<String>): Any {
    val grid = lines.map { it.toList() }
    val bounds = grid.bounds()

    return bounds.allWithinBounds().filter {
        grid[it] != '.'
    }.groupBy {
        grid[it]
    }.values.flatMap { antennas ->
        antennas.permutations(2).map { (a1, a2) ->
            a1 + (a1 - a2)
        }.filter { it.withinBounds(bounds) }
    }.toSet().size
}


fun day08_2(lines: List<String>): Any {
    val grid = lines.map { it.toList() }
    val bounds = grid.bounds()

    return bounds.allWithinBounds().filter {
        grid[it] != '.'
    }.groupBy {
        grid[it]
    }.values.flatMap { antennas ->
        antennas.permutations(2).flatMap { (a1, a2) ->
            infiniteSequence().map {
                a1 + (a1 - a2) * it
            }.takeWhile { it.withinBounds(bounds) }
        }
    }.toSet().size
}

fun main() {

//    run("1", fileName = "day08_ex.txt", func = ::day08_1)
    run("2", fileName = "day08_ex.txt", func = ::day08_2)


//    run("1", fileName = "day08.txt", func = ::day08_1)
    run("2", fileName = "day08.txt", func = ::day08_2)
}
