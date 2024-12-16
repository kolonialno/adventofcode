package com.matsemann.adventofcode2024

import com.matsemann.adventofcode2024.utils.*
import com.matsemann.adventofcode2024.utils.IntVec.Companion.allWithinBounds


fun day16_1(lines: List<String>): Any {
    val (grid, bounds) = lines.toCharGrid()
    val start = bounds.allWithinBounds().first { grid[it] == 'S' }

    val dij = Dijkstra<Pair<IntVec, Direction>> { (pos, dir) ->
        listOfNotNull(
            if (grid[pos + dir] != '#') pos + dir to dir to 1 else null,
            pos to dir.turnCw() to 1000,
            pos to dir.turnCcw() to 1000,
        )
    }

    return dij.solve(start to Direction.RIGHT) {
        grid[it.first] == 'E'
    }?.let { (_, cost) -> cost } ?: "failed"
}


fun day16_2(lines: List<String>): Any {
    return 2
}

fun main() {
    Direction.setYDown()

    run("1", fileName = "day16_ex.txt", func = ::day16_1)
//    run("2", fileName = "day16_ex.txt", func = ::day16_2)


    run("1", fileName = "day16.txt", func = ::day16_1)
//    run("2", fileName = "day16.txt", func = ::day16_2)
}
