package com.matsemann.adventofcode2024

import com.matsemann.adventofcode2024.utils.*
import com.matsemann.adventofcode2024.utils.IntVec.Companion.allBetween
import com.matsemann.adventofcode2024.utils.IntVec.Companion.allWithinBounds
import com.matsemann.adventofcode2024.utils.IntVec.Companion.showAsGrid


fun day15_1(lines: List<String>): Any {
    val (map, moves) = lines.splitBy { it == "" }
    Direction.setYDown()

    val (grid, bounds) = map.toCharGrid()
    val mutableGrid = grid.toMutableList()

    moves.flatMap { it.toList() }.forEach { move ->
        val dir = Direction.fromArrows(move)
        val pos = bounds.allWithinBounds().first { mutableGrid[it] == '@' }
        val nextPos = pos + dir

        if (mutableGrid[nextPos] == 'O') {
            val untilNotBox = infiniteSequence().map {  nextPos + dir.toVec()*it }.first {
                !it.withinBounds(bounds) || mutableGrid[it] != 'O'
            }
            if (mutableGrid[untilNotBox] == '.') {
                untilNotBox.allBetween(pos).zipWithNext().forEach { (a, b) ->
                    mutableGrid[a] = mutableGrid[b]
                }
                mutableGrid[pos] = '.'
            }
        } else if (mutableGrid[nextPos] == '.') {
            mutableGrid[nextPos] = '@'
            mutableGrid[pos] = '.'
        }

    }

    println(bounds.showAsGrid { mutableGrid[it] })

    return bounds.allWithinBounds().filter { mutableGrid[it] == 'O' }.sumOf { it.y * 100 + it.x }
}


fun day15_2(lines: List<String>): Any {
    return 2
}

fun main() {

    run("1", fileName = "day15_ex.txt", func = ::day15_1)
//    run("2", fileName = "day15_ex.txt", func = ::day15_2)


    run("1", fileName = "day15.txt", func = ::day15_1)
//    run("2", fileName = "day15.txt", func = ::day15_2)
}
