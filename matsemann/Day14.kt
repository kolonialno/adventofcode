package com.matsemann.adventofcode2022

import com.matsemann.adventofcode2022.utils.*
import com.matsemann.adventofcode2022.utils.IntVec.Companion.bounds
import com.matsemann.adventofcode2022.utils.IntVec.Companion.showAsGrid

fun day14_1(lines: List<String>): Any {
    val paths = lines.map { it.split(" -> ") }
        .map { splits ->
            splits.map { IntVec.fromStr(it) }
        }

    val rocks = paths.flatMap { path ->
        path.zipWithNext().flatMap { (v1, v2) ->
            val diff = v1.chebyshev(v2)
            val dir = (v2 - v1).asDir()
            (0..diff).map { v1 + (dir * it) }
        }.toSet()
    }.toSet()

    val lowest = rocks.maxOf { it.y }
    val sandStart = IntVec(500, 0)

    val grid = rocks.toMutableSet()
    val dirs = listOf(IntVec(0, 1), IntVec(-1, 1), IntVec(1, 1))

    var giveUp = false
    var sandsPlaced = 0
    while (!giveUp) {
        var sand = sandStart

        while (true) {
            if (sand.y > lowest) {
                giveUp = true
                break
            }

            val dir = dirs.firstOrNull { sand + it !in grid }
            if (dir == null) {
                grid.add(sand)
                sandsPlaced++
                break
            } else {
                sand += dir
            }
        }

//        println()
//        println((grid + sand).showAsGrid())
//        println()

    }

    return sandsPlaced
}


fun day14_2(lines: List<String>): Any {
    val paths = lines.map { it.split(" -> ") }
        .map { splits ->
            splits.map { IntVec.fromStr(it) }
        }

    val rocks = paths.flatMap { path ->
        path.zipWithNext().flatMap { (v1, v2) ->
            val diff = v1.chebyshev(v2)
            val dir = (v2 - v1).asDir()
            (0..diff).map { v1 + (dir * it) }
        }.toSet()
    }.toSet()

    val bounds = rocks.bounds()
    val lowest = bounds[3]
    val sandStart = IntVec(500, 0)

    val floor = ((450 - lowest)..(550 + lowest)).map { IntVec(it, lowest + 2) }

    val grid = (rocks + floor).toMutableSet()
    val dirs = listOf(IntVec(0, 1), IntVec(-1, 1), IntVec(1, 1))

    var giveUp = false
    var sandsPlaced = 0

    while (!giveUp) {
        var sand = sandStart

        while (true) {
            val dir = dirs.firstOrNull { sand + it !in grid }
            if (dir == null) {
                grid.add(sand)
                sandsPlaced++

                if (sand == sandStart) {
                    giveUp = true
                }
                break
            } else {
                sand += dir
            }
        }
//        println()
//        println((grid + sand).showAsGrid())
//        println()

    }


    return sandsPlaced
}

fun main() {

    run("1", fileName = "day14_ex.txt", func = ::day14_1)
    run("2", fileName = "day14_ex.txt", func = ::day14_2)

    run("1", fileName = "day14.txt", func = ::day14_1)
    run("2", fileName = "day14.txt", func = ::day14_2)
}

/*
OUTPUT
======

Result for 1 (day14.txt):	715

Result for 2 (day14.txt):	25248
 */