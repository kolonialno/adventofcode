package com.matsemann.adventofcode2022


fun day02_1(lines: List<String>): Any {
    return lines
        .map { it.split(" ") }
        .map { (move1, move2) ->
            when (move1[0] to move2[0]) {
                'A' to 'X' -> 1 + 3
                'A' to 'Y' -> 2 + 6
                'A' to 'Z' -> 3 + 0
                'B' to 'X' -> 1 + 0
                'B' to 'Y' -> 2 + 3
                'B' to 'Z' -> 3 + 6
                'C' to 'X' -> 1 + 6
                'C' to 'Y' -> 2 + 0
                'C' to 'Z' -> 3 + 3
                else -> 0
            }
        }.sum()
}

fun day02_2(lines: List<String>): Any {
    return lines
        .map { it.split(" ") }
        .map { (move1, move2) ->
            when (move1[0] to move2[0]) {
                'A' to 'X' -> 3 + 0
                'A' to 'Y' -> 1 + 3
                'A' to 'Z' -> 2 + 6
                'B' to 'X' -> 1 + 0
                'B' to 'Y' -> 2 + 3
                'B' to 'Z' -> 3 + 6
                'C' to 'X' -> 2 + 0
                'C' to 'Y' -> 3 + 3
                'C' to 'Z' -> 1 + 6
                else -> 0
            }
        }.sum()
}

fun main() {

//    run("1", fileName = "day02_ex.txt", func = ::day02_1)
//    run("2", fileName = "day02_ex.txt", func = ::day02_2)

    run("1", fileName = "day02.txt", func = ::day02_1)
    run("2", fileName = "day02.txt", func = ::day02_2)
}

