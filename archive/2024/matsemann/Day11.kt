package com.matsemann.adventofcode2024

import com.matsemann.adventofcode2024.utils.*


val memoizedSolver = Cached2<_, _, Long> { num: Long, blinksLeft: Long ->
    if (blinksLeft == 0L) {
        1
    } else if (num == 0L) {
        this(1, blinksLeft - 1)
    } else if (num.toString().length % 2 == 0) {
        val str = num.toString()
        val left = str.take(str.length / 2).toLong()
        val right = str.drop(str.length / 2).toLong()
        this(left, blinksLeft - 1) + this(right, blinksLeft - 1)
    } else {
        this(num * 2024, blinksLeft - 1)
    }
}

fun day11_1(lines: List<String>): Any {
    return lines.first().split(" ").longs().sumOf { memoizedSolver(it, 25) }
}

fun day11_2(lines: List<String>): Any {
    return lines.first().split(" ").longs().sumOf { memoizedSolver(it, 75) }
}

fun main() {
    run("1", fileName = "day11_ex.txt", func = ::day11_1)
    run("2", fileName = "day11_ex.txt", func = ::day11_2)

    run("1", fileName = "day11.txt", func = ::day11_1)
    run("2", fileName = "day11.txt", func = ::day11_2)
}
