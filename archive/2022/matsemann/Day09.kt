package com.matsemann.adventofcode2022

import kotlin.math.abs

fun day09_1(lines: List<String>): Any {
    val moves = lines
        .map { it.split(" ") }
        .map { (ch, num) -> Direction.fromUDLR(ch) to num.toInt() }

    var head = IntVec.zero
    var tail = IntVec.zero

    val positions = mutableSetOf(tail)

    moves.forEach { (dir, num) ->
        repeat(num) {
            head += dir
            val dst = head - tail
            if (abs(dst.y) == 2 || abs(dst.x) == 2) {
                tail += dst.asDir()
                positions.add(tail)
            }
        }
    }


    return positions.size
}

fun day09_2(lines: List<String>): Any {
    val moves = lines
        .map { it.split(" ") }
        .map { (ch, num) -> Direction.fromUDLR(ch) to num.toInt() }

    val knots = MutableList(10) { IntVec.zero }
    val positions = mutableSetOf(knots[9])

    moves.forEach { (dir, num) ->
        repeat(num) {
            knots[0] = knots[0] + dir
            for (i in 1 until knots.size) {
                val dst = knots[i-1] - knots[i]
                if (dst.chebyshev() > 1) {
                    knots[i] += dst.asDir()
                }
            }
            positions.add(knots[9])
        }
    }
    return positions.size
}

fun main() {

//    run("1", fileName = "day09_ex.txt", func = ::day09_1)
    run("2", fileName = "day09_ex.txt", func = ::day09_2)

//    run("1", fileName = "day09.txt", func = ::day09_1)
    run("2", fileName = "day09.txt", func = ::day09_2)
}

/*
OUTPUT
======


 */