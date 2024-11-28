package com.matsemann.adventofcode2021

fun day11_1(lines: List<String>): Any {

    val grid = lines.map { line -> line.map { it.digitToInt() }.toMutableList() }.toMutableList()
    val bounds = IntVec(lines.first().length - 1, lines.size - 1)

    val allPos = grid.indices.flatMap { y ->
        grid[y].indices.map { x -> IntVec(x, y) }
    }
    return (0 until 100).sumOf {
        allPos.forEach {
            grid[it] = grid[it] + 1
        }
        val hasFlashed = mutableSetOf<IntVec>()
        val toCheck = allPos.toMutableList()

        while (toCheck.isNotEmpty()) {
            val first = toCheck.removeFirst()
            if (first in hasFlashed) {
                continue
            }
            if (grid[first] > 9) {
                val neighbors = first.neighbors9().filter { it.withinBounds(bounds) }
                neighbors.forEach {
                    grid[it] = grid[it] + 1
                }
                toCheck.addAll(neighbors)
                hasFlashed.add(first)
            }
        }

        hasFlashed.forEach {
            grid[it] = 0
        }

        hasFlashed.size
    }
}

// Basically a copy of part1
fun day11_2(lines: List<String>): Any {

    val grid = lines.map { line -> line.map { it.digitToInt() }.toMutableList() }.toMutableList()
    val bounds = IntVec(lines.first().length - 1, lines.size - 1)

    val allPos = grid.indices.flatMap { y ->
        grid[y].indices.map { x -> IntVec(x, y) }
    }

    var count = 0
    while (true) {
        allPos.forEach {
            grid[it] = grid[it] + 1
        }
        val hasFlashed = mutableSetOf<IntVec>()
        val toCheck = allPos.toMutableList()

        while (toCheck.isNotEmpty()) {
            val first = toCheck.removeFirst()
            if (first in hasFlashed) {
                continue
            }
            if (grid[first] > 9) {
                val neighbors = first.neighbors9().filter { it.withinBounds(bounds) }
                neighbors.forEach {
                    grid[it] = grid[it] + 1
                }
                toCheck.addAll(neighbors)
                hasFlashed.add(first)
            }
        }

        hasFlashed.forEach {
            grid[it] = 0
        }
        if (hasFlashed.size == 100) {
            return count + 1
        }
        count++
    }
}

fun main() {
//    run("1", fileName = "day11_ex.txt", func = ::day11_1)
    run("1", fileName = "day11_1.txt", func = ::day11_1)
//    run("2", fileName = "day11_ex.txt", func = ::day11_2)
    run("2", fileName = "day11_1.txt", func = ::day11_2)
}

/*
OUTPUT
======

Done. Took 3ms to run
Result for 1:	1585
Copied to clipboard!

Done. Took 9ms to run
Result for 2:	382
Copied to clipboard!

 */