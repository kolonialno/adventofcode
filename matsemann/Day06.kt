package com.matsemann.adventofcode2022


fun day06_1(lines: List<String>): Any {
    return lines[0].windowed(4).indexOfFirst {
        it.toSet().size == 4
    } + 4
}

fun day06_2(lines: List<String>): Any {
    return lines[0].windowed(14).indexOfFirst {
        it.toSet().size == 14
    } + 14
}

fun main() {

//    run("1", fileName = "day06_ex.txt", func = ::day06_1)
//    run("2", fileName = "day06_ex.txt", func = ::day06_2)

    run("1", fileName = "day06.txt", func = ::day06_1)
    run("2", fileName = "day06.txt", func = ::day06_2)
}
