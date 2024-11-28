package ren.iamka.aoc23.day6

import ren.iamka.aoc23.readLines
import ren.iamka.aoc23.toPositiveInts

fun main() {
    parsePart1()
    parsePart2()
}


private fun parsePart1() {
    return "/day6/data.txt".readLines {
        val (times, distances) = this.toList().map { it.toPositiveInts() }
        val timesToDistances = times.zip(distances)
        println(timesToDistances)
        timesToDistances.map { (time, distance) ->
            (0..time).map { charging ->
                charging * (time - charging)
            }.count { it > distance }
        }.reduce { acc, i ->
            acc * i
        }.apply { println(this) }
    }
}

private fun parsePart2() {
    return "/day6/data.txt".readLines {
        val (time, distance) = this.toList().map { it.filter { char -> char.isDigit() }.toLong() }
        (0..time).map { charging ->
            charging * (time - charging)
        }.count { it > distance }.apply { println(this) }
    }
}