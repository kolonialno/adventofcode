package com.matsemann.adventofcode2021

fun day09_1(lines: List<String>): Any {
    val grid = lines.map { line -> line.map { it.digitToInt() } }
    val bounds = IntVec(lines.first().length - 1, lines.size -1)

    return (0..bounds.x).flatMap { x ->
        (0..bounds.y).map { y -> IntVec(x, y) } // a vector for each position
    }.filter { pos ->
        pos.neighbors(bounds) // for all neighbors on the grid, check if
            .all { neighbor -> // it's the lowest point of them, then it's a low point
                grid[neighbor] > grid[pos]
            }
    }.sumOf { pos ->
        grid[pos] + 1
    }
}


fun day09_2(lines: List<String>): Any {
    val grid = lines.map { line -> line.map { it.digitToInt() } }
    val bounds = IntVec(lines.first().length - 1, lines.size -1)

    return (0..bounds.x).flatMap { x ->
        (0..bounds.y).map { y -> IntVec(x, y) }
    }.filter { pos ->
        val neighbors = pos.neighbors(bounds)
        neighbors.all { neighbor ->
            grid[neighbor] > grid[pos]
        }
    }.map { pos ->
        // For all the low points, floodfill outwards using a
        // bfs  to find how many we visit
        // implemented from the faaar back of my head, happy
        // with how fast I actually managed to do it and that it worked 😂
        val visited = mutableSetOf<IntVec>()
        val toVisit = mutableListOf(pos)

        while (toVisit.isNotEmpty()) {
            val next = toVisit.removeFirst()
            if (next in visited) {
                continue
            }
            visited.add(next)
            val neighbors = next.neighbors(bounds)
                .filter { neighbor ->
                    grid[neighbor] >= grid[next]
                }
                .filterNot { neighbor -> grid[neighbor] == 9 }
            toVisit.addAll(neighbors)
        }

        visited.size

    }.sorted().takeLast(3).reduce(Int::times)
}

operator fun <E> List<List<E>>.get(intVec: IntVec) = this[intVec.y][intVec.x]

fun main() {
//    run("1", fileName = "day09_ex.txt", func = ::day09_1)
    run("1", fileName = "day09_1.txt", func = ::day09_1)
//    run("2", fileName = "day09_ex.txt", func = ::day09_2)
    run("2", fileName = "day09_1.txt", func = ::day09_2)
}

/*
OUTPUT
======

Done. Took 3ms to run
Result for 1:	580
Copied to clipboard!

Done. Took 9ms to run
Result for 2:	856716
Copied to clipboard!

 */