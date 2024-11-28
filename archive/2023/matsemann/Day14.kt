package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*
import com.matsemann.adventofcode2023.utils.IntVec.Companion.allWithinBounds

fun day14_1(lines: List<String>): Any {
    val grid = lines.map { it.toList() }.toMutableList()
    val bounds = grid.bounds()

    val rocks = bounds.allWithinBounds().filter { grid[it] == 'O' }
    val dir = Direction.DOWN

    val newRocks = rocks.map { rock ->
        var pos = rock
        var newPos = rock + dir
        while (newPos.withinBounds(bounds) && grid[newPos] == '.') {
            pos = newPos
            newPos = pos + dir
        }
        grid[rock] = '.'
        grid[pos]= 'O'
        pos
    }

    return newRocks.map {
        (bounds.y + 1) - it.y
    }.sum()

}


fun day14_2(lines: List<String>): Any {
    val grid = lines.map { it.toList() }.toMutableList()
    val bounds = grid.bounds()

    var rocks = bounds.allWithinBounds().filter { grid[it] == 'O' }
    grid.map { it.joinToString("") { it.toString() } }.println()

    val previousRocks = mutableListOf<Set<IntVec>>()

    val goal = 1000000000
    for(i in 0..<goal) {
        for (dir in listOf(Direction.DOWN, Direction.LEFT, Direction.UP, Direction.RIGHT)) {
            // Sort to avoid colliding with rocks in front
            rocks = when(dir) {
                Direction.RIGHT -> rocks.sortedBy { -it.x }
                Direction.DOWN -> rocks.sortedBy { it.y }
                Direction.LEFT -> rocks.sortedBy { it.x }
                Direction.UP -> rocks.sortedBy { -it.y }
            }

            rocks = rocks.map { rock ->
                var pos = rock
                var newPos = rock + dir
                while (newPos.withinBounds(bounds) && grid[newPos] == '.') {
                    pos = newPos
                    newPos = pos + dir
                }
                grid[rock] = '.'
                grid[pos]= 'O'
                pos
            }
        }

        // Find when the pattern repeats, and calculate which
        // in that pattern we would've landed on
        if (rocks.toSet() in previousRocks) {
            val prevHit = previousRocks.indexOf(rocks.toSet())
            val period = i - prevHit
            val jumps = (((goal-1) - i) / period)
            val jumpsTo = i + jumps * period
            val diff = goal - 1 - jumpsTo
            val goalPrev = prevHit + diff
            println("goal is $goalPrev")
            rocks = previousRocks[prevHit + diff].toList()
            break
        }
        previousRocks += rocks.toSet()
    }

    return rocks.map {
        (bounds.y + 1) - it.y
    }.sum()
}

fun main() {

//    run("1", fileName = "day14_ex.txt", func = ::day14_1)
    run("2", fileName = "day14_ex.txt", func = ::day14_2)


//    run("1", fileName = "day14.txt", func = ::day14_1)
    run("2", fileName = "day14.txt", func = ::day14_2)
}
