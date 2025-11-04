package com.matsemann.adventofcode2024

import com.matsemann.adventofcode2024.utils.*
import com.matsemann.adventofcode2024.utils.Direction.*
import com.matsemann.adventofcode2024.utils.IntVec.Companion.allWithinBounds


fun day12_1(lines: List<String>): Any {
    val (grid, bounds) = lines.toCharGrid()

    return bounds.allWithinBounds()
        .fold(emptySet<IntVec>() to emptyList<Set<IntVec>>()) { (seen, groups), pos ->
            if (pos in seen) {
                seen to groups
            } else {
                val bfs = BFS<IntVec> { it.neighbors(bounds).filter { grid[it] == grid[pos] } }
                bfs.solve(pos)
                val newGroups = groups + listOf(bfs.visited)
                (seen + bfs.visited) to newGroups
            }
        }.second.map { group ->
            group.size *
            group.sumOf { pos -> pos.neighbors().count { !it.withinBounds(bounds) || grid[it] != grid[pos] } }
        }.sum()

}


fun day12_2(lines: List<String>): Any {
    val (grid, bounds) = lines.toCharGrid()

    return bounds.allWithinBounds()
        .fold(emptySet<IntVec>() to emptyList<Set<IntVec>>()) { (seen, groups), pos ->
            if (pos in seen) {
                seen to groups
            } else {
                val bfs = BFS<IntVec> { it.neighbors(bounds).filter { grid[it] == grid[pos] } }
                bfs.solve(pos)
                val newGroups = groups + listOf(bfs.visited)
                (seen + bfs.visited) to newGroups
            }
        }.second.map { group ->
            val dirs = listOf(UP, RIGHT, DOWN, LEFT).zip(listOf(
                compareBy<IntVec> {it.x},
                compareBy<IntVec> {it.x},
                compareBy<IntVec> {it.x},
                compareBy<IntVec> {it.x},
            ))

            listOf<IntVec>().sortedWith(compareBy {it.x})

            group.size *
                    group.sumOf { pos -> pos.neighbors().count { !it.withinBounds(bounds) || grid[it] != grid[pos] } }
        }.sum()
}

fun main() {

    run("1", fileName = "day12_ex.txt", func = ::day12_1)
//    run("2", fileName = "day12_ex.txt", func = ::day12_2)


    run("1", fileName = "day12.txt", func = ::day12_1)
//    run("2", fileName = "day12.txt", func = ::day12_2)
}
