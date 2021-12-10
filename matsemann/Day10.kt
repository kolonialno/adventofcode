package com.matsemann.adventofcode2021

import java.math.BigInteger

// It's stupid but it worked 😂
// removes pairs again and again until it cannot find anything more to remove
// (<{}> => (<> => (
fun removePairs(line: String): String {
    return generateSequence(line) { prev ->
        listOf("()", "[]", "<>", "{}").fold(prev) { acc, pair ->
            acc.replace(pair, "")
        }
    }.zipWithNext().takeWhile { it.first != it.second }.last().second
}

fun day10_1(lines: List<String>): Any {
    val points = mapOf(
        ')' to 3,
        ']' to 57,
        '}' to 1197,
        '>' to 25137
    )

    return lines
        .map { removePairs(it) }
        .mapNotNull { it.toCharArray().firstOrNull { char -> char in points.keys } }
        .sumOf { points[it]!! }

}

fun day10_2(lines: List<String>): Any {
    val points = mapOf(
        ')' to 1,
        ']' to 2,
        '}' to 3,
        '>' to 4
    )
    val opposite = mapOf(
        '<' to '>',
        '(' to ')',
        '[' to ']',
        '{' to '}'
    )

    return lines
        .map { removePairs(it) }
        .filterNot { it.toCharArray().any { char -> char in points.keys } }
        .map { it.reversed() }
        .map { remaining ->
            remaining.fold(0.toBigInteger()) { acc, char ->
                acc * 5 + points[opposite[char]]!!
            }
        }
        .sorted()
        .let { it[it.size / 2] }


}

operator fun BigInteger.times(other: Int): BigInteger = this.multiply(other.toBigInteger())
operator fun BigInteger.plus(other: Int): BigInteger = this.add(other.toBigInteger())


fun main() {
//    run("1", fileName = "day10_ex.txt", func = ::day10_1)
    run("1", fileName = "day10_1.txt", func = ::day10_1)
//    run("2", fileName = "day10_ex.txt", func = ::day10_2)
    run("2", fileName = "day10_1.txt", func = ::day10_2)
}

/*
OUTPUT
======

Done. Took 0ms to run
Result for 1:	315693
Copied to clipboard!

Done. Took 0ms to run
Result for 2:	1870887234
Copied to clipboard!

 */