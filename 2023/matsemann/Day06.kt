package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*

fun day06_1(lines: List<String>): Any {
    return lines
        .map {
            it.allInts()
        }
        .transpose()
        .map { (time, distance) ->
            (0..time).count { holdTime ->
                (time - holdTime) * holdTime > distance
            }
        }.product()
}

fun day06_2(lines: List<String>): Any {
    return lines.map {
        it.replace(" ", "").split(":")[1].toLong()
    }.let { (time, distance) ->
        (0L..time).count { holdTime ->
            (time - holdTime) * holdTime > distance
        }
    }
}

fun main() {

//    run("1", fileName = "day06_ex.txt", func = ::day06_1)
    run("2", fileName = "day06_ex.txt", func = ::day06_2)


//    run("1", fileName = "day06.txt", func = ::day06_1)
    run("2", fileName = "day06.txt", func = ::day06_2)
}
