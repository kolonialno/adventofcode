package com.matsemann.adventofcode2021

fun solve(lines: List<String>, days: Int): Any {
    // Solves it in linear time ( O(days) ) by only keeping
    // count of totals, not modeling each fish
    var state = Array(9) { 0.toBigInteger() }

    lines.first()
        .split(",")
        .map { it.toInt() }
        .forEach { state[it]++ }

    for (d in 1..days) {
        val nextState = Array(9) { 0.toBigInteger() }

        for (i in nextState.indices) {
            val fish = state[i]

            if (i == 0) {
                nextState[8] += fish
                nextState[6] += fish
            } else {
                nextState[i-1] += fish
            }
        }

        state = nextState
    }

    return state.sumOf { it }
}



fun main() {
    for (i in 0..100) {
        run("1", fileName = "day06_1.txt") {
            solve(it, 80)
        }
        run("2", fileName = "day06_1.txt") {
            solve(it, 256)
        }
    }
}

/*
OUTPUT
======

Done. Took 0ms to run
Result for 1:	350149
Copied to clipboard!

Done. Took 0ms to run
Result for 2:	1590327954513
Copied to clipboard!

 */