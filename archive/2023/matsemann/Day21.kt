package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*
import com.matsemann.adventofcode2023.utils.IntVec.Companion.allWithinBounds
import com.matsemann.adventofcode2023.utils.IntVec.Companion.showAsGrid

fun day21_1(lines: List<String>): Any {
    val grid = lines.map { it.toList() }
    val bounds = grid.bounds()
    val startPos = bounds.allWithinBounds().first { grid[it] == 'S' }

    val bfs = BFS<IntVec> {
        it.neighbors(bounds).filter { n -> grid[n] == '.' }
    }
    bfs.solve(startPos) {
        path(it).size == 64 + 2
    }

    println(bounds.showAsGrid { if (it in bfs.visited) 'O' else grid[it] })
    println()

    val stepsAway = bfs.visited.filter { bfs.path(it).size % 2 == 1 }

    println(bounds.showAsGrid { if (it in stepsAway) 'O' else grid[it] })
    return stepsAway.size
}


fun day21_2(lines: List<String>): Any {
    val grid = lines.map { it.toList() }
    val bounds = grid.bounds()
    val startPos = bounds.allWithinBounds().first { grid[it] == 'S' }

    val bfs = BFS<IntVec> {node ->
        node.neighbors()
//            .map { n -> IntVec(n.x % bounds.x, n.y % bounds.y) }
            .filter { n ->
                val intVec = IntVec((n.x).mod(bounds.x+1), (n.y).mod(bounds.y+1))
                grid[intVec] == '.' || grid[intVec] == 'S'
            }
    }

//    repeat(30) {steps ->
//        val s = steps * 10
//        bfs.solve(startPos) {
//            path(it).size == s + 2
//        }
//        val stepsAway = bfs.visited.filter { bfs.path(it).size % 2 == 1 }
//        println("$s\t${stepsAway.size}")
//    }
    // Hmm looks quadratic
    // excel gives me a quadratic that is close
    // but doesn't align on integers

    // Try with multiples of the input size
//    repeat(10) {steps ->
//        val s = steps * bounds.x
//        bfs.solve(startPos) {
//            path(it).size == s + 2
//        }
//        val stepsAway = bfs.visited.filter { bfs.path(it).size % 2 == 1 }
//        println("$s\t${stepsAway.size}")
//    }

    // Soo close, average predictions off by ~10. But still no exact match in excel.
    // not sure if to use bounds.x or (bounds.x+1)

    // The output pattern reaches the edge in 64 moves. So we have a repeat then on the next move?
//    repeat(10) {steps ->
//        val s =  steps * 65
//        bfs.solve(startPos) {
//            path(it).size == s + 2
//        }
//        val stepsAway = bfs.visited.filter { bfs.path(it).size % 2 == 1 }
//        println("$s\t${stepsAway.size}")
//    }
    // Hmm no sigar. But only the first one is special, so let's do the boundary one but start with the pattern size
//    repeat(10) {steps ->
//        val s =  steps * (bounds.x+1) + 65
//        bfs.solve(startPos) {
//            path(it).size == s + 2
//        }
//        val stepsAway = bfs.visited.filter { bfs.path(it).size % 2 == 1 }
//        println("$s\t${stepsAway.size}")
//    }
//
    // oh fuck I think it gives wrong answers when odd numbers
//    repeat(10) {steps ->
//        val s =  steps * (bounds.x+1) + 65
//        bfs.solve(startPos) {
//            path(it).size == s + 2
//        }
//
//        // fix steps away filter and keep the correct ones when steps is odd
//        val mod = if (s % 2 == 1) 0 else 1
//        val stepsAway = bfs.visited.filter { bfs.path(it).size % 2 == mod}
//        println("$s\t${stepsAway.size}")
//    }
    // Yesss it gives an exact quadratic
    // y = 15186x^2 + 15276x + 3848

    val steps = 26501365
    val x = ((steps - 65) / (bounds.x+1)).toLong()

    return 15186*x*x + 15276*x + 3848
}

fun main() {

//    run("1", fileName = "day21_ex.txt", func = ::day21_1)
//    run("2", fileName = "day21_ex.txt", func = ::day21_2)


//    run("1", fileName = "day21.txt", func = ::day21_1)
    run("2", fileName = "day21.txt", func = ::day21_2)
}
