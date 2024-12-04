package com.matsemann.adventofcode2024

import com.matsemann.adventofcode2024.utils.*
import com.matsemann.adventofcode2024.utils.IntVec.Companion.allBetween
import com.matsemann.adventofcode2024.utils.IntVec.Companion.allWithinBounds


fun day04_1(lines: List<String>): Any {
    val grid = lines.map { it.toList() }
    val bounds = grid.bounds()

    val directions = IntVec.zero.neighbors9().map { it * 3 }
    val search = "XMAS".toList()

    val Xs = bounds.allWithinBounds().filter { grid[it] == 'X' }

    return Xs.sumOf { x ->
        directions.count { dir ->
            x.allBetween(x + dir).filter {
                it.withinBounds(bounds)
            }.map {
                grid[it]
            } == search
        }
    }
}


fun day04_2(lines: List<String>): Any {
    val grid = lines.map { it.toList() }
    val bounds = grid.bounds()

    val xDirections = listOf(
        listOf(IntVec(-1, -1), IntVec(1, 1)),
        listOf(IntVec(-1, 1), IntVec(1, -1))
    )
    val search = listOf("MAS".toList(), "SAM".toList())

    return bounds.allWithinBounds().filter {
        grid[it] == 'A'
    }.count { a ->
        xDirections.all { dir ->
            (a + dir[0]).allBetween(a + dir[1]).filter {
                it.withinBounds(bounds)
            }.map {
                grid[it]
            } in search
        }
    }
}

fun main() {

//    run("1", fileName = "day04_ex.txt", func = ::day04_1)
    run("2", fileName = "day04_ex.txt", func = ::day04_2)


//    run("1", fileName = "day04.txt", func = ::day04_1)
    run("2", fileName = "day04.txt", func = ::day04_2)
}
