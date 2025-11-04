package com.matsemann.adventofcode2024

import com.matsemann.adventofcode2024.utils.*
import com.matsemann.adventofcode2024.utils.IntVec.Companion.allWithinBounds


fun day10_1(lines: List<String>): Any {
    val grid = lines.map { it.toList() }
    val bounds = grid.bounds()

    return bounds.allWithinBounds().filter { grid[it] == '0' }.sumOf { start ->
        val bfs = BFS<IntVec> { pos -> pos.neighbors(bounds).filter { grid[it] == grid[pos] + 1 }}
        bfs.solve(start)
        bfs.visited.count { grid[it] == '9' }
    }

}


fun day10_2(lines: List<String>): Any {
    val grid = lines.map { it.toList() }
    val bounds = grid.bounds()

    fun dfs(pos: IntVec): Int {
        if (grid[pos] == '9') return 1
        return pos.neighbors(bounds).filter { grid[it] == grid[pos] + 1 }.sumOf { dfs(it) }
    }

    return bounds.allWithinBounds().filter { grid[it] == '0' }.sumOf { dfs(it) }
}

fun main() {

//    run("1", fileName = "day10_ex.txt", func = ::day10_1)
    run("2", fileName = "day10_ex.txt", func = ::day10_2)


//    run("1", fileName = "day10.txt", func = ::day10_1)
    run("2", fileName = "day10.txt", func = ::day10_2)
}
