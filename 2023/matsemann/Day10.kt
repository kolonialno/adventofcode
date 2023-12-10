package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*
import com.matsemann.adventofcode2023.utils.IntVec.Companion.allWithinBounds
import com.matsemann.adventofcode2023.utils.IntVec.Companion.showAsGrid

val `|` = listOf(Direction.UP, Direction.DOWN)
val `-` = listOf(Direction.LEFT, Direction.RIGHT)
val L = listOf(Direction.DOWN, Direction.RIGHT)
val J = listOf(Direction.DOWN, Direction.LEFT)
val F = listOf(Direction.UP, Direction.RIGHT)
val `7` = listOf(Direction.UP, Direction.LEFT)
fun day10_1(lines: List<String>): Any {
    var sPos = IntVec.zero
    val grid = lines.mapIndexed { y, line ->
        line.toList().mapIndexed { x, c ->
            if (c == 'S') {
                sPos = IntVec(x, y)
            }
            when (c) {
                '|' -> `|`
                '-' -> `-`
                'L' -> L
                'J' -> J
                '7' -> `7`
                'F' -> F
                else -> listOf()
            }
        }
    }.toMutableList()

    // Figure out where S points, by looking at what points at S
    grid[sPos] = Direction.entries.mapNotNull { dir ->
        val newPos = sPos + dir
        if (newPos.withinBounds(grid.bounds()) && dir.flip() in grid[newPos]) {
            dir
        } else {
            null
        }
    }

    // Simple BFS from S
    val Q = mutableListOf(sPos)
    val visited = mutableSetOf<IntVec>()

    while (Q.isNotEmpty()) {
        val currentPos = Q.removeFirst()
        if (currentPos in visited) {
            continue
        }
        visited += currentPos
        grid[currentPos].forEach { dir ->
            Q.add(currentPos + dir)
        }
    }


    return visited.size / 2
}


fun day10_2(lines: List<String>): Any {
    var sPos = IntVec.zero
    val grid = lines.mapIndexed { y, line ->
        line.toList().mapIndexed { x, c ->
            if (c == 'S') {
                sPos = IntVec(x, y)
            }
            when (c) {
                '|' -> `|`
                '-' -> `-`
                'L' -> L
                'J' -> J
                '7' -> `7`
                'F' -> F
                else -> listOf()
            }
        }
    }.toMutableList()

    // Figure out where S points, by looking at what points at S
    grid[sPos] = Direction.entries.mapNotNull { dir ->
        val newPos = sPos + dir
        if (newPos.withinBounds(grid.bounds()) && dir.flip() in grid[newPos]) {
            dir
        } else {
            null
        }
    }

    // Simple BFS from S
    val Q = mutableListOf(sPos)
    val visited = mutableSetOf<IntVec>()

    while (Q.isNotEmpty()) {
        val currentPos = Q.removeFirst()
        if (currentPos in visited) {
            continue
        }
        visited += currentPos
        grid[currentPos].forEach { dir ->
            Q.add(currentPos + dir)
        }
    }

    // Do a "point in polygon test" to the right edge, it's inside if count%2==1
    // worst case is stuff like .F-7F-J|  (.┏━┓┏┛┃)
    // where we are outside, but only cross one | when going right. Or also cross odd number if counting corners.
    // So have to see that F later followed by a J changes if we're in/out, but a F later followed by a 7 doesn't
    val bounds = grid.bounds()
    val inside = bounds.allWithinBounds().filter { pos ->
        pos !in visited // don't check things on the main pipe
    }.filter { pos ->
        // All pipes from the main pipe we pass through going right, excluding -
        val passes = (pos.x..bounds.x).map { x ->
            IntVec(x, pos.y)
        }.filter { posToCheck ->
            posToCheck in visited
        }.map { posToCheck -> grid[posToCheck]
        }.filter { pipe -> pipe != `-` }

        // Count if we're inside or not, by making sure F7 counts twice, but FJ only once
        // (so we count the F, but on J we don't count if previous was F)
        passes.mapIndexed { i, pipe ->
            val prev = if (i > 0) passes[i - 1] else null
            when (pipe to prev) {
                J to F -> 0
                `7` to L -> 0
                else -> 1
            }
        }.sum() % 2 == 1
    }//.println()

    // Debug grid :D
    val realGrid = lines.map { it.toList() }
    val st = (0..bounds.y).joinToString("\n") { y ->
        (0..bounds.x).joinToString("") { x ->
            val pos = IntVec(x, y)
            if (pos in visited) {
                val s = realGrid[pos]
                when (s) {
                    'F' -> "┏"
                    'J' -> "┛"
                    'L' -> "┗"
                    '7' -> "┓"
                    '|' -> "┃"
                    '-' -> "━"
                    'S' -> "S"
                    else -> " "
                }
            } else if (pos in inside) {
                "I"
            } else {
                "."
            }
        }
    }
    println()
    println(st)

    return inside.count()
}

fun main() {

//    run("1", fileName = "day10_ex.txt", func = ::day10_1)
//    run("1", fileName = "day10_ex2.txt", func = ::day10_1)
//    run("2", fileName = "day10_ex.txt", func = ::day10_2)
    run("2", fileName = "day10_ex3.txt", func = ::day10_2)
    // => length = 46, areal = 50, så innesluttet = 4
    run("2", fileName = "day10_ex4.txt", func = ::day10_2)
    // => length = 44, areal = 48, så innesluttet = 4
    run("2", fileName = "day10_ex2.txt", func = ::day10_2)
    run("2", fileName = "day10_ex5.txt", func = ::day10_2)
    // => 10


//    run("1", fileName = "day10.txt", func = ::day10_1)
    run("2", fileName = "day10.txt", func = ::day10_2)

}
