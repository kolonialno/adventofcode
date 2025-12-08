package com.matsemann.adventofcode2025

import com.matsemann.adventofcode2025.utils.*


fun day07_1(lines: List<String>): Any {
    val splitters = lines.map { line -> line.withIndex().filter { it.value == '^' }.map { it.index } }
    val start = lines.first().indexOf('S')

    return splitters
        .fold(setOf(start) to 0) { (beams, numSplits), split ->
            val splits = split.filter { it in beams }.toSet()
            val newBeams = beams.flatMap { b ->
                if (b in splits) {
                    listOf(b - 1, b + 1)
                } else {
                    listOf(b)
                }
            }.toSet()
            newBeams to numSplits + splits.size
        }.second
}


fun day07_2(lines: List<String>): Any {
    /**
     * From top to bottom
     * start with [0, 0, 0, 1, 0, 0, 0]
     * and increase left/right side of ^'s as we encounter them
     */
    val splitters = lines.map { line -> line.withIndex().filter { it.value == '^' }.map { it.index } }
    val start = lines.first().indexOf('S')

    val row = List(lines.first().length) { if (it == start) 1L else 0L }

    return splitters.fold(row) { prevRow, splits ->
        prevRow.mapIndexed { index, prevVal ->
            var newVal = 0L
            if (index !in splits) {
                newVal += prevVal
            }
            if (index + 1 in splits) {
                newVal += prevRow[index+1]
            }
            if (index -1 in splits) {
                newVal += prevRow[index-1]
            }
            newVal
        }
    }.sum()
}

fun day07_2_recursive(lines: List<String>): Any {
    /**
     * Same but with dynamic programming
     * Start at bottom, sum of three values above (depending on if ^ or not),
     * memoized+recursive
     */
    val (grid, bounds) = lines.toCharGrid()

    val algo = Cached { loc: IntVec ->
        if (loc.y == 0) {
            if (grid[loc] == 'S') 1L else 0L
        } else {
            listOfNotNull(
                Direction.UP.takeIf { grid[loc] != '^' },
                Direction.UPLEFT.takeIf { loc.x > 0 && grid[loc + Direction.LEFT] == '^' },
                Direction.UPRIGHT.takeIf { loc.x < bounds.x  && grid[loc + Direction.RIGHT] == '^' },
            ).sumOf { this(loc + it) }
        }
    }

    return (0..bounds.x).sumOf {
        algo(IntVec(it, bounds.y))
    }
}

fun main() {
    Direction.setYDown()

//    run("1", fileName = "day07_ex.txt", func = ::day07_1)
//    run("2", fileName = "day07_ex.txt", func = ::day07_2)
//    run("2", fileName = "day07_ex.txt", func = ::day07_2_recursive)


//    run("1", fileName = "day07.txt", func = ::day07_1)
    run("2", fileName = "day07.txt", func = ::day07_2)
    run("2", fileName = "day07.txt", func = ::day07_2_recursive)
}
