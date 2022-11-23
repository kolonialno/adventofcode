package com.matsemann.adventofcode2021

fun main() {
    run("1", fileName = "day01_1.txt") { lines ->
        lines
            .map { it.toInt() }
            .zipWithNext { a, b -> a < b }
            .count { it }
    }
    run("2", fileName = "day01_1.txt") { lines ->
        lines
            .map { it.toInt() }
            .windowed(3) { it.sum() }
            .zipWithNext { a, b -> a < b }
            .count { it }
    }
}

/*

OUTPUT
======

Done. Took 21ms to run
Result for 1:	1462

Done. Took 24ms to run
Result for 2:	1497

 */