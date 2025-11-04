package com.matsemann.adventofcode2024

import com.matsemann.adventofcode2024.utils.*
import com.matsemann.adventofcode2024.utils.IntVec.Companion.allWithinBounds


fun day06_1(lines: List<String>): Any {

    val grid = lines.map { it.toList() }
    val bounds = grid.bounds()

    val start = bounds.allWithinBounds().first { grid[it] == '^' }

    val visits = mutableSetOf(start)
    var pos = start
    var direction = Direction.UP
    Direction.setYDown()

    while ((pos + direction).withinBounds(bounds)) {
        if (grid[pos + direction] == '#') {
            direction = direction.turnCw()
        }
        pos += direction
        visits.add(pos)
    }

    return visits.size
}


fun day06_2(lines: List<String>): Any {

    val grid = lines.map { it.toList() }
    val bounds = grid.bounds()

    val start = bounds.allWithinBounds().first { grid[it] == '^' }

    Direction.setYDown()

    fun check(extraObstruction: IntVec): Boolean {
        var pos = start
        var direction = Direction.UP
        val visits = mutableSetOf(start to direction)

        while ((pos + direction).withinBounds(bounds)) {
            if (grid[pos + direction] == '#' || pos + direction == extraObstruction) {
                direction = direction.turnCw()
                continue // hack to handle corners where multiple turns needed, so just iterate again
            }
            pos += direction

            if (visits.contains(pos to direction)) {
                return true
            }

            visits.add(pos to direction)
        }
        return false
    }

    return bounds.allWithinBounds().filter {
        grid[it] == '.'
    }.filter {
        check(it)
    }
//        .println()
        .count()
}

fun main() {

//    run("1", fileName = "day06_ex.txt", func = ::day06_1)
    run("2", fileName = "day06_ex.txt", func = ::day06_2)


//    run("1", fileName = "day06.txt", func = ::day06_1)
    run("2", fileName = "day06.txt", func = ::day06_2)
}
