package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*
import kotlin.math.abs

fun day11_1(lines: List<String>): Any {

    val galaxies = lines.flatMapIndexed { i, line ->
        line.toList().mapIndexed { j, c ->
            if (c == '#') IntVec(j, i) else null
        }.filterNotNull()
    }
    val spaceRows = lines.runningFold(0) { prev, row ->
        if (row.all { it == '.' }) prev + 1
        else prev
    }.drop(1)

    val spaceCols = lines.map { it.toList() }.transpose().runningFold(0) { prev, row ->
        if (row.all { it == '.' }) prev + 1
        else prev
    }.drop(1)

    return galaxies.combinations(length = 2).map { (g1, g2) ->
        val dst = g1.manhattan(g2).toBigInteger()
        val expandRow = abs(spaceRows[g2.y] - spaceRows[g1.y]) * (1000000 - 1)
        val expandCol = abs(spaceCols[g2.x] - spaceCols[g1.x]) *  (1000000 - 1)
        dst + expandRow + expandCol
    }.sumOf { it }

}


fun day11_2(lines: List<String>): Any {
    val galaxies = lines.flatMapIndexed { i, line ->
        line.toList().mapIndexed { j, c ->
            if (c == '#') IntVec(j, i) else null
        }.filterNotNull()
    }
    val spaceRows = lines.runningFold(0) { prev, row ->
        if (row.all { it == '.' }) prev + 1
        else prev
    }.drop(1).map { it.toBigInteger() }

    val spaceCols = lines.map { it.toList() }.transpose().runningFold(0) { prev, row ->
        if (row.all { it == '.' }) prev + 1
        else prev
    }.drop(1).map { it.toBigInteger() }

    return galaxies.combinations(length = 2).map { (g1, g2) ->
        val dst = g1.manhattan(g2).toBigInteger()
        val expandRow = (spaceRows[g2.y] - spaceRows[g1.y]).abs() * (1000000 - 1)
        val expandCol = (spaceCols[g2.x] - spaceCols[g1.x]).abs() *  (1000000 - 1)
        dst + expandRow + expandCol
    }.sumOf { it }
}

fun main() {

    run("1", fileName = "day11_ex.txt", func = ::day11_1)
//    run("2", fileName = "day11_ex.txt", func = ::day11_2)


    run("1", fileName = "day11.txt", func = ::day11_1)
//    run("2", fileName = "day11.txt", func = ::day11_2)
}
