package com.matsemann.adventofcode2025

import com.matsemann.adventofcode2025.utils.*
import com.matsemann.adventofcode2025.utils.IntVec.Companion.allWithinBounds


fun day04_1(lines: List<String>): Any {
    val grid = lines.map { it.toList() }
    val bounds = grid.bounds()
    return bounds.allWithinBounds().filter { loc ->
        grid[loc] == '@'
    }.filter { loc ->
        loc.neighbors9(bounds).count { n ->
            grid[n] == '@'
        } < 4
    }.count()
}


fun day04_2(lines: List<String>): Any {
    val grid = lines.map { it.toList() }.toMutableList()
    val bounds = grid.bounds()
    val Q = bounds.allWithinBounds().filter { loc ->
        grid[loc] == '@'
    }.toMutableList()

    var count = 0

    while (Q.isNotEmpty()) {
        val loc = Q.removeFirst()
        if (grid[loc] != '@') {
            continue
        }
        val toiletNeighbors = loc.neighbors9(bounds).filter { n ->
            grid[n] == '@'
        }
        if (toiletNeighbors.size < 4) {
            count++
            grid[loc] = '.'
            Q.addAll(toiletNeighbors)
        }
    }

    return count
}

fun main() {

//    run("1", fileName = "day04_ex.txt", func = ::day04_1)
    run("2", fileName = "day04_ex.txt", func = ::day04_2)


//    run("1", fileName = "day04.txt", func = ::day04_1)
    run("2", fileName = "day04.txt", func = ::day04_2)
}
