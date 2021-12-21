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

    data class Param(val myPos: Int, val myScore: Int, val otherPos: Int, val otherScore: Int)
    val play = Cached<Param, Pair<BigInteger, BigInteger>> { (myPos, myScore, otherPos, otherScore) ->
        possible3Rolls.map { (roll, count) ->
            val newPos = (myPos + roll) % 10
            val newScore = myScore + newPos + 1

            if (newScore >= 21) {
                count.toBigInteger() to BigInteger.ZERO
            } else {
                val nextResult = this(Param(otherPos, otherScore, newPos, newScore))
                nextResult.second * count to nextResult.first * count
            }
        }.reduce { acc, p -> acc.first + p.first to acc.second + p.second }
    }

    return play(Param(startP1 - 1, 0, startP2 - 1, 0)).let {
        maxOf(it.first, it.second)
    }
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

Done. Took 0ms to run
Result for 1:	906093
Copied to clipboard!

Done. Took 23ms to run
Result for 2:	274291038026362
Copied to clipboard!

 */