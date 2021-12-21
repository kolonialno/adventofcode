package com.matsemann.adventofcode2021

import java.math.BigInteger

fun day21_1(startP1: Int, startP2: Int): Any {
    class DeterministicDie {
        var rolls = 0
        operator fun invoke(): Int {
            return rolls++ % 100 + 1
        }
    }

    val die = DeterministicDie()

    var p1Pos = startP1 - 1
    var p1Score = 0

    var p2Pos = startP2 - 1
    var p2Score = 0

    while (true) {
        var roll = die() + die() + die()
        p1Pos = (p1Pos + roll) % 10
        p1Score += p1Pos + 1
        if (p1Score >= 1000) {
            return p2Score * die.rolls
        }

        roll = die() + die() + die()
        p2Pos = (p2Pos + roll) % 10
        p2Score += p2Pos + 1
        if (p2Score >= 1000) {
            return p1Score * die.rolls
        }
    }
}

fun day21_2(startP1: Int, startP2: Int): Any {

    val possibleRolls = listOf(1, 2, 3)
    val possible3Rolls = cartesian(listOf(possibleRolls, possibleRolls, possibleRolls))
        .map { it.sum() }
        .groupingBy { it }.eachCount()

    fun play(pos: List<Int>, scores: List<Int>, turn: Int): Pair<BigInteger, BigInteger> {
        return possible3Rolls.map { (roll, count) ->
            val newPos = pos.toMutableList()
            val newScores = scores.toMutableList()

            newPos[turn] = (pos[turn] + roll) % 10
            newScores[turn] = scores[turn] + newPos[turn] + 1

            if (newScores[turn] >= 21) {
                if (turn == 0)
                    count.toBigInteger() to BigInteger.ZERO
                else
                    BigInteger.ZERO to count.toBigInteger()
            } else {
                val nextResult = play(newPos, newScores, (turn + 1) % 2)
                nextResult.first * count to nextResult.second * count
            }
        }.reduce { acc, p -> acc.first + p.first to acc.second + p.second }
    }

    val result = play(listOf(startP1 - 1, startP2 - 1), listOf(0, 0), 0)
    return maxOf(result.first, result.second)
}

fun main() {

    run("1", fileName = "day21_ex.txt") {
        day21_1(10, 7)
    }
    run("2", fileName = "day21_ex.txt") {
        day21_2(10, 7)
    }
}

/*
OUTPUT
======

Done. Took 1ms to run
Result for 1:	906093
Copied to clipboard!

Done. Took 6229ms to run
Result for 2:	274291038026362
Copied to clipboard!

 */