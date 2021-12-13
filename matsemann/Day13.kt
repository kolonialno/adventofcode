package com.matsemann.adventofcode2021

import com.matsemann.adventofcode2021.IntVec.Companion.toIntVec

fun day13_1(lines: List<String>): Any {
    val initialPoints = lines.filter { it.contains(",") }.map { it.toIntVec() }

    val instructions = lines.filter { it.contains("fold along") }.map { it.split(" ")[2] }
        .map {
            val a = it.split("=")
            a[0] to a[1].toInt()
        }


    val result = instructions
        .take(1)
        .fold(initialPoints) { points, fold ->
            points.map { point ->
                if (fold.first == "y" && point.y > fold.second) {
                    IntVec(point.x, fold.second - (point.y - fold.second))
                } else if (fold.first == "x" && point.x > fold.second) {
                    IntVec(fold.second - (point.x - fold.second), point.y)
                } else {
                    point
                }
            }.distinct()
        }

    return result.size

}


fun day13_2(lines: List<String>): Any {
    val initialPoints = lines.filter { it.contains(",") }.map { it.toIntVec() }

    val instructions = lines.filter { it.contains("fold along") }.map { it.split(" ")[2] }
        .map {
            val a = it.split("=")
            a[0] to a[1].toInt()
        }


    val result = instructions
        .fold(initialPoints) { points, fold ->
        points.map { point ->
            if (fold.first == "y" && point.y > fold.second) {
                IntVec(point.x, fold.second - (point.y - fold.second))
            } else if (fold.first == "x" && point.x > fold.second) {
                IntVec(fold.second - (point.x - fold.second), point.y)
            } else {
                point
            }
        }.distinct()
    }

    (0..10).forEach { y ->
        (0..50).forEach { x ->
            if (IntVec(x, y) in result) {
                print("#")
            } else {
                print(".")
            }
        }
        println()
    }

    return ""
}

fun main() {
//    run("1", fileName = "day13_ex.txt", func = ::day13_1)
    run("1", fileName = "day13_1.txt", func = ::day13_1)
//    run("2", fileName = "day13_ex.txt", func = ::day13_2)
    run("2", fileName = "day13_1.txt", func = ::day13_2)
}

/*
OUTPUT
======

Done. Took 1ms to run
Result for 1:	775
Copied to clipboard!

###..####.#..#.###..#..#.###..#..#.###.............
#..#.#....#..#.#..#.#..#.#..#.#.#..#..#............
#..#.###..#..#.#..#.#..#.#..#.##...#..#............
###..#....#..#.###..#..#.###..#.#..###.............
#.#..#....#..#.#....#..#.#....#.#..#.#.............
#..#.####..##..#.....##..#....#..#.#..#............
...................................................
...................................................
...................................................
...................................................
...................................................
Done. Took 3ms to run
Result for 2:
Copied to clipboard!

 */