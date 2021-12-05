package com.matsemann.adventofcode2021

import com.matsemann.adventofcode2021.IntVec.Companion.toIntVec

fun solve(strLines: List<String>, filter: Boolean) =
    strLines
        .map {
            it.split(" ").let { parts ->
                Line(parts[0].toIntVec(), parts[2].toIntVec())
            }
        }
        .filter {
            !filter || (it.p1.x == it.p2.x || it.p1.y == it.p2.y)
        }
        .flatMap { line ->
            val diff = line.p2 - line.p1
            val dir = diff.asDir()
            (0..diff.chebyshev()).map { i ->
                line.p1 + dir * i
            }
        }
        .groupingBy { it }
        .eachCount()
        .count { it.value >= 2 }


fun main() {
    run("1", fileName = "day05_1.txt") {
        solve(it, filter = true)
    }
    run("2", fileName = "day05_1.txt") {
        solve(it, filter = false)
    }
}

data class Line(val p1: IntVec, val p2: IntVec)

/*
OUTPUT
======

Done. Took 156ms to run
Result for 1:	5698
Copied to clipboard!

Done. Took 218ms to run
Result for 2:	15463
Copied to clipboard!

 */