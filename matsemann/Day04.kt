package com.matsemann.adventofcode2022

import com.matsemann.adventofcode2022.IntVec.Companion.toIntVec


fun day04_1(lines: List<String>): Any {
    return lines.map { it.split(",") }
        .map { (first, second) ->
            first.toIntVec("-") to second.toIntVec("-")
        }.count { (first, second) ->
            first.x >= second.x && first.y <= second.y
                    || second.x >= first.x && second.y <= first.y
        }
}

fun day04_2(lines: List<String>): Any {
    return lines.map { it.split(",") }
        .map { (first, second) ->
            first.toIntVec("-") to second.toIntVec("-")
        }.count { (first, second) ->
            first.y >= second.x && first.x <= second.y
        }
}


fun day04_1_2(lines: List<String>): Any {
    return lines
        .map { it.split(",", "-").ints() }
        .count { (x1, y1, x2, y2) ->
            x1 >= x2 && y1 <= y2
                    || x2 >= x1 && y2 <= y1
        }
}

fun day04_2_2(lines: List<String>): Any {
    return lines
        .map { it.split(",", "-").ints() }
        .count { (x1, y1, x2, y2) ->
            y1 >= x2 && x1 <= y2
        }
}


fun main() {

//    run("1", fileName = "day04_ex.txt", func = ::day04_1)
//    run("2", fileName = "day04_ex.txt", func = ::day04_2)

//    run("1", fileName = "day04.txt", func = ::day04_1)
    run("2", fileName = "day04.txt", func = ::day04_2)
}

/*
OUTPUT
======


 */