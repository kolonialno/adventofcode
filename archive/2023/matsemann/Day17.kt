package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*
import com.matsemann.adventofcode2023.utils.IntVec.Companion.showAsGrid

fun day17_1(lines: List<String>): Any {
    val grid = lines.map { it.toList().map { it.digitToInt().toLong() } }
    val bounds = grid.bounds()

    data class State(val pos: IntVec, val dir: Direction, val straights: Int)

    val dijkstra = Dijkstra<State> {(pos, dir, straights) ->
        val straight = if (straights  < 3 && (pos+dir).withinBounds(bounds)) {
            State(pos + dir, dir, straights + 1) to grid[pos+dir]
        } else {
            null
        }
        val leftDir = dir.turnCcw()
        val left = if ((pos+leftDir).withinBounds(bounds)) {
            State(pos + leftDir, leftDir, 1) to grid[pos+leftDir]
        } else {
            null
        }
        val rightDir = dir.turnCw()
        val right = if ((pos+rightDir).withinBounds(bounds)) {
            State(pos + rightDir, rightDir, 1) to grid[pos+rightDir]
        } else {
            null
        }

        listOfNotNull(straight, left, right)
    }
    val res = dijkstra.solve(State(IntVec(0,0), Direction.RIGHT, 0)) { (pos, dir, straights) ->
        pos == bounds
    }

    val path = dijkstra.path(res!!.first)
    val cost = path.map { grid[it.pos] }.sum() - grid[IntVec(0,0)]

    return cost
}


fun day17_2(lines: List<String>): Any {
    val grid = lines.map { it.toList().map { it.digitToInt().toLong() } }
    val bounds = grid.bounds()

    data class State(val pos: IntVec, val dir: Direction, val straights: Int)

    val dijkstra = Dijkstra<State> {(pos, dir, straights) ->
        val straight = if (straights  < 10 && (pos+dir).withinBounds(bounds)) {
            State(pos + dir, dir, straights + 1) to grid[pos+dir]
        } else {
            null
        }
        val leftDir = dir.turnCcw()
        val left = if (straights > 3 && (pos+leftDir).withinBounds(bounds)) {
            State(pos + leftDir, leftDir, 1) to grid[pos+leftDir]
        } else {
            null
        }
        val rightDir = dir.turnCw()
        val right = if (straights > 3 && (pos+rightDir).withinBounds(bounds)) {
            State(pos + rightDir, rightDir, 1) to grid[pos+rightDir]
        } else {
            null
        }

        listOfNotNull(straight, left, right)
    }
    // Now start direction matters, but simpler to just try UP and RIGHT
    // and see which one is shorter, than adding support for two start nodes in my
    // Dijkstra
    val res = dijkstra.solve(State(IntVec(0,0), Direction.UP, 0)) { (pos, dir, straights) ->
        pos == bounds && straights > 3
    }

    val path = dijkstra.path(res!!.first)

    println(path.map { it.pos }.showAsGrid())
    return path.map { grid[it.pos] }.sum() - grid[IntVec(0,0)]
}

fun main() {

//    run("1", fileName = "day17_ex.txt", func = ::day17_1)
    run("2", fileName = "day17_ex.txt", func = ::day17_2)
    run("2", fileName = "day17_ex2.txt", func = ::day17_2)


//    run("1", fileName = "day17.txt", func = ::day17_1)
    // 1304 too high
    run("2", fileName = "day17.txt", func = ::day17_2)
}
