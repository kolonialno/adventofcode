package com.matsemann.adventofcode2022

import com.matsemann.adventofcode2022.utils.*
import com.matsemann.adventofcode2022.utils.IntVec.Companion.allWithinBounds

fun bfs(grid: List<List<Char>>, start: IntVec, goal: IntVec): Int {
    val bounds = grid.bounds()

    val parents = mutableMapOf<IntVec, IntVec>()
    val explored = mutableSetOf(start)
    val queue = mutableListOf(start)

    while (queue.isNotEmpty()) {
        val next = queue.removeFirst()

        if (next == goal) {
            break
        }

        next.neighbors(bounds)
            .filter { grid[it] <= grid[next] + 1 }
            .filter { it !in explored }
            .forEach {
                parents[it] = next
                explored.add(it)
                queue.add(it)
            }
    }

    var count = -1
    var current: IntVec? = goal
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

    return bfs(grid, start, goal)
}


fun day12_2(lines: List<String>): Any {
    val grid = lines.map { it.toList() }.toMutableList()
    val positions = grid.bounds().allWithinBounds()

    val start = positions.first { grid[it] == 'S' }
    val goal = positions.first { grid[it] == 'E' }


    grid[start] = 'a'
    grid[goal] = 'z'

    return positions
        .filter { grid[it] == 'a' }
        .map { bfs(grid, it, goal) }
        .filter { it > 0 }
        .min()

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