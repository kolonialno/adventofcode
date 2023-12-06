package ren.iamka.aoc23.day6

import ren.iamka.aoc23.readLines
import ren.iamka.aoc23.toInts

fun main() {
    parse()
}


private fun parse() {
    return "/day6/data.txt".readLines {
        val (times, distances) = this.toList().map { it.toInts() }
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