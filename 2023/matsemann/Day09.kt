package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*

fun day09_1(lines: List<String>): Any {
    return lines.map { line ->
        generateSequence { }.runningFold(line.allInts()) { prev, _ ->
            prev.zipWithNext { a, b -> b - a }
        }.takeWhile { seq ->
            seq.any { it != 0 }
        }.fold(0) { acc, curr -> acc + curr.last() }
    }.sum()
}


fun day09_2(lines: List<String>): Any {
    return lines.map { line ->
        generateSequence { }.runningFold(line.allInts()) { prev, _ ->
            prev.zipWithNext { a, b -> b - a }
        }.takeWhile { seq ->
            seq.any { it != 0 }
        }.toList().foldRight(0) { curr, acc -> curr.first() - acc }
    }.sum()
}

fun main() {

//    run("1", fileName = "day09_ex.txt", func = ::day09_1)
    run("2", fileName = "day09_ex.txt", func = ::day09_2)


//    run("1", fileName = "day09.txt", func = ::day09_1)
    run("2", fileName = "day09.txt", func = ::day09_2)
}
