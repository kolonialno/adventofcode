package com.matsemann.adventofcode2022

import com.matsemann.adventofcode2022.utils.*
import com.matsemann.adventofcode2022.utils.IntVec.Companion.allWithinBounds

fun bfs(grid: List<List<Char>>, start: IntVec, neighbors: (IntVec, IntVec) -> Boolean, goal: (IntVec) -> Boolean): Int {
    val bounds = grid.bounds()

    val parents = mutableMapOf<IntVec, IntVec>()
    val explored = mutableSetOf(start)
    val queue = mutableListOf(start)

    var endGoal: IntVec? = null

    while (queue.isNotEmpty()) {
        val next = queue.removeFirst()

        if (goal(next)) {
            endGoal = next
            break
        }

        next.neighbors(bounds)
            .filter { neighbors(next, it) }
            .filter { it !in explored }
            .forEach {
                parents[it] = next
                explored.add(it)
                queue.add(it)
            }
    }

    var count = -1
    var current: IntVec? = endGoal
    while (current != null) {
        count++
        current = parents[current]
    }

    return count
}

fun day12_1(lines: List<String>): Any {
    val grid = lines.map { it.toList() }.toMutableList()
    val positions = grid.bounds().allWithinBounds()

    val start = positions.first { grid[it] == 'S' }
    val goal = positions.first { grid[it] == 'E' }
    grid[start] = 'a'
    grid[goal] = 'z'

    return bfs(grid, start, { a, b -> grid[b] <= grid[a] + 1 }, { it == goal })
}


fun day12_2(lines: List<String>): Any {
    val grid = lines.map { it.toList() }.toMutableList()
    val positions = grid.bounds().allWithinBounds()

    val start = positions.first { grid[it] == 'S' }
    val goal = positions.first { grid[it] == 'E' }

    grid[start] = 'a'
    grid[goal] = 'z'

    return bfs(grid, goal,
        { a, b -> grid[a] <= grid[b] + 1 },
        { grid[it] == 'a' }
    )

}

fun main() {

//    run("1", fileName = "day12_ex.txt", func = ::day12_1)
//    run("2", fileName = "day12_ex.txt", func = ::day12_2)

    run("1", fileName = "day12.txt", func = ::day12_1)
    run("2", fileName = "day12.txt", func = ::day12_2)
}

/*
OUTPUT
======


 */