package com.matsemann.adventofcode2022


fun day01_1(lines: List<String>): Any {
    return lines
        .splitBy { it == "" }
        .map { strings -> strings.sumOf { it.toInt() } }
        .max()
}


fun day01_2(lines: List<String>): Any {
    return lines
        .splitBy { it == "" }
        .map { strings -> strings.sumOf { it.toInt() } }
        .sortedDescending()
        .take(3)
        .sum()
}

fun main() {

//    run("1", fileName = "day01_ex.txt", func = ::day01_1)
//    run("2", fileName = "day01_ex.txt", func = ::day01_2)

    run("1", fileName = "day01.txt", func = ::day01_1)
    run("2", fileName = "day01.txt", func = ::day01_2)
}
