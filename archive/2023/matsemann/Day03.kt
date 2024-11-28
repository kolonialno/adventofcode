package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*
import com.matsemann.adventofcode2023.utils.IntVec.Companion.allWithinBounds

fun day03_1(lines: List<String>): Any {
    val grid = lines.map { it.toList() }
    val bounds = grid.bounds()

    val numbers = mutableListOf<MutableList<IntVec>>()
    bounds.allWithinBounds().forEach { pos ->
        if (pos.x == 0) { // new row
            numbers += mutableListOf<IntVec>()
        }
        if (grid[pos].isDigit()) {
            numbers.last() += pos
        } else if (numbers.last().isNotEmpty()) {
            numbers += mutableListOf<IntVec>()
        }
    }

    val numbersAdjacent = numbers.filter { number ->
        number.any { pos ->
            pos
                .neighbors9()
                .filter { it.withinBounds(bounds) }
                .any { grid[it] != '.' && !grid[it].isDigit() }
        }
    }

    return numbersAdjacent.sumOf { number -> number.joinToString("") { grid[it].toString() }.toInt() }
}


fun day03_2(lines: List<String>): Any {
    val grid = lines.map { it.toList() }
    val bounds = grid.bounds()

    val numbers = mutableListOf<MutableList<IntVec>>()
    bounds.allWithinBounds().forEach { pos ->
        if (pos.x == 0) { // new row
            numbers += mutableListOf<IntVec>()
        }
        if (grid[pos].isDigit()) {
            numbers.last() += pos
        } else if (numbers.last().isNotEmpty()) {
            numbers += mutableListOf<IntVec>()
        }
    }

    val gears = bounds.allWithinBounds()
        .filter { grid[it] == '*' }

    return gears
        .map { gear ->
            // Find all adjacent numbers for each gear
            numbers.filter { number -> number.any { it.neighbors9().any { it == gear } } }
        }
        .filter { neighborNumbers -> neighborNumbers.size == 2 }
        .map { neighborNumbers ->
            neighborNumbers.map { number -> number.map { grid[it].digitToInt() }.concat() }.product()
        }
        .sum()

}

fun main() {

//    run("1", fileName = "day03_ex.txt", func = ::day03_1)
    run("2", fileName = "day03_ex.txt", func = ::day03_2)


//    run("1", fileName = "day03.txt", func = ::day03_1)
    run("2", fileName = "day03.txt", func = ::day03_2)
}
