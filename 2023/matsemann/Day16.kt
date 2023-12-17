package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*
import com.matsemann.adventofcode2023.utils.Direction.*

fun day16_1(lines: List<String>): Any {
    val grid = lines.map { it.toList() }
    val bounds = grid.bounds()
    val res = generateSequence { }.scan(
        listOf(
            IntVec(
                0,
                0
            ) to RIGHT
        ) to setOf<Pair<IntVec, Direction>>()
    ) { (beams, seenStates), _ ->
        val newBeams = beams.flatMap { (pos, dir) ->
            if (!pos.withinBounds(bounds) || (pos to dir) in seenStates) {
                emptyList()
            } else {
                val value = grid[pos]
                when {
                    value == '|' && dir in listOf(LEFT, RIGHT) -> listOf((pos + DOWN) to DOWN, (pos + UP) to UP)
                    value == '-' && dir in listOf(DOWN, UP) -> listOf((pos + LEFT) to LEFT, (pos + RIGHT) to RIGHT)
                    value == '/' && dir == RIGHT -> listOf((pos + UP) to UP)
                    value == '/' && dir == DOWN -> listOf((pos + LEFT) to LEFT)
                    value == '/' && dir == LEFT -> listOf((pos + DOWN) to DOWN)
                    value == '/' && dir == UP -> listOf((pos + RIGHT) to RIGHT)
                    value == '\\' && dir == RIGHT -> listOf((pos + DOWN) to DOWN)
                    value == '\\' && dir == DOWN -> listOf((pos + RIGHT) to RIGHT)
                    value == '\\' && dir == LEFT -> listOf((pos + UP) to UP)
                    value == '\\' && dir == UP -> listOf((pos + LEFT) to LEFT)
                    else -> listOf((pos + dir) to dir)
                }
            }
        }

        newBeams to (seenStates + beams)
    }.dropWhile { (beams, seenStates) -> beams.isNotEmpty() }.first()

    val seenPos = res.second.map { it.first }.filter { it.withinBounds(bounds) }
//    println(seenPos.showAsGrid())
    return seenPos.toSet().size
}


fun day16_2(lines: List<String>): Any {
    val grid = lines.map { it.toList() }
    val bounds = grid.bounds()

    val vertical = (0..bounds.y).flatMap { y -> listOf(IntVec(0, y) to RIGHT, IntVec(bounds.x, y) to LEFT) }
    val horizontal = (0..bounds.x).flatMap { x -> listOf(IntVec(x, 0) to DOWN, IntVec(x, bounds.y) to UP) }

    return (vertical + horizontal).maxOf { startPos ->
        generateSequence { }.scan(listOf(startPos) to setOf<Pair<IntVec, Direction>>()) { (beams, seenStates), _ ->
            val newBeams = beams.flatMap { (pos, dir) ->
                if (!pos.withinBounds(bounds) || (pos to dir) in seenStates) {
                    emptyList()
                } else {
                    val value = grid[pos]
                    when {
                        value == '|' && dir in listOf(LEFT, RIGHT) -> listOf((pos + DOWN) to DOWN, (pos + UP) to UP)
                        value == '-' && dir in listOf(DOWN, UP) -> listOf(
                            (pos + LEFT) to LEFT,
                            (pos + RIGHT) to RIGHT
                        )

                        value == '/' && dir == RIGHT -> listOf((pos + UP) to UP)
                        value == '/' && dir == DOWN -> listOf((pos + LEFT) to LEFT)
                        value == '/' && dir == LEFT -> listOf((pos + DOWN) to DOWN)
                        value == '/' && dir == UP -> listOf((pos + RIGHT) to RIGHT)
                        value == '\\' && dir == RIGHT -> listOf((pos + DOWN) to DOWN)
                        value == '\\' && dir == DOWN -> listOf((pos + RIGHT) to RIGHT)
                        value == '\\' && dir == LEFT -> listOf((pos + UP) to UP)
                        value == '\\' && dir == UP -> listOf((pos + LEFT) to LEFT)
                        else -> listOf((pos + dir) to dir)
                    }
                }
            }

            newBeams to (seenStates + beams)
        }.dropWhile { (beams, seenStates) -> beams.isNotEmpty() }
            .first().second
            .map { it.first }.filter { it.withinBounds(bounds) }.toSet()
            .size
    }
}

fun main() {
    Direction.setYDown()

//    run("1", fileName = "day16_ex.txt", func = ::day16_1)
    run("2", fileName = "day16_ex.txt", func = ::day16_2)


//    run("1", fileName = "day16.txt", func = ::day16_1)
    run("2", fileName = "day16.txt", func = ::day16_2)
}
