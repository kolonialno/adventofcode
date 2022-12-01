package com.matsemann.adventofcode2022

fun day01_1(lines: List<String>): Any {
    val elves = Counter<Int>()
    var index = 0
    for (line in lines) {
        if (line == "") {
            index++
            continue
        }
        elves[index] += line.toBigInteger()
    }

    return elves.values.max()
}


fun day01_2(lines: List<String>): Any {
    val elves = Counter<Int>()
    var index = 0
    for (line in lines) {
        if (line == "") {
            index++
            continue
        }
        elves[index] += line.toBigInteger()
    }

    return elves.values.sortedDescending().subList(0, 3).sumOf { it.toInt() }

}

fun main() {

//    run("1", fileName = "day01_ex.txt", func = ::day01_1)
//    run("2", fileName = "day01_ex.txt", func = ::day01_2)

//    run("1", fileName = "day01.txt", func = ::day01_1)
    run("2", fileName = "day01.txt", func = ::day01_2)
}
