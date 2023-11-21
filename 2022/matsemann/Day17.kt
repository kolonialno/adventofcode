package com.matsemann.adventofcode2022

import com.matsemann.adventofcode2022.utils.*
import com.matsemann.adventofcode2022.utils.IntVec.Companion.showAsGrid
import kotlin.math.max

fun day17_1(lines: List<String>): Any {
    val winds = lines.first().map { if (it == '<') Direction.LEFT else Direction.RIGHT }

    val floor = (0..8).map { IntVec(it, 0) }
    val walls = (0..10_000).flatMap { listOf(IntVec(0, it), IntVec(8, it)) }

    val grid = (floor + walls).toMutableSet()

    var windIndex = 0
    var tetraIndex = 0

    var highest = 0

    for (i in 0 until 2022) {
        val tetra = tetraminos.circular(tetraIndex++)

        var tetraPos = IntVec(3, highest+4)

        while (true) {
            val wind = winds.circular(windIndex++)

            val windCollides = checkCollision(tetraPos + wind, tetra, grid)
            tetraPos = if (!windCollides) {
                tetraPos + wind
            } else {
                tetraPos
            }

            val downCollides = checkCollision(tetraPos + Direction.DOWN, tetra, grid)
            if (downCollides) {
                val elements = tetra.map { it + tetraPos }
                grid.addAll(elements)

                val top = elements.maxOf { it.y }
                highest = max(top, highest)
                break
            } else {
                tetraPos += Direction.DOWN
            }

        }

//        println()
//        println(grid.showAsGrid().split("\n").reversed().joinToString("\n"))
    }

    return highest
}

fun checkCollision(pos: IntVec, tetramino: Set<IntVec>, grid: Set<IntVec>): Boolean {
    return tetramino.map { it + pos }.any { it in grid }
}


val tetraminos = listOf(
    setOf(IntVec(0, 0), IntVec(1, 0), IntVec(2, 0), IntVec(3, 0)), // -
    setOf(IntVec(1, 0), IntVec(0, 1), IntVec(1, 1), IntVec(2, 1), IntVec(1, 2)), // +
    setOf(IntVec(0, 0), IntVec(1, 0), IntVec(2, 0), IntVec(2, 1), IntVec(2, 2)), // _|
    setOf(IntVec(0, 0), IntVec(0, 1), IntVec(0, 2), IntVec(0, 3)), // |
    setOf(IntVec(0, 0), IntVec(1, 0), IntVec(0, 1), IntVec(1, 1)), // []
)

fun day17_2(lines: List<String>): Any {
    val winds = lines.first().map { if (it == '<') Direction.LEFT else Direction.RIGHT }
    println("winds: ${winds.size}")

    val floor = (0..8).map { IntVec(it, 0) }
    val walls = (0..5).flatMap { listOf(IntVec(0, it), IntVec(8, it)) }

    val grid = (floor + walls).toMutableSet()

    var windIndex = 0
    var tetraIndex = 0

    var highest = 0

    val heights = mutableListOf<Int>()
    for (i in 0 until winds.size * tetraminos.size * 1000) {
//        if (i % (winds.size * tetraminos.size) == 0) {
        if (i % (winds.size * tetraminos.size*345) == 4_100_200) {
//            println()
//            println("$i, highest: $highest")
            heights.add(highest)

//            val top = grid.filter { it.y > highest -3 }
//            println(top.showAsGrid().split("\n").reversed().joinToString("\n"))
//            println()
        }
        val tetra = tetraminos.circular(tetraIndex++)

        var tetraPos = IntVec(3, highest+4)

        while (true) {
            val wind = winds.circular(windIndex++)

            val windCollides = checkCollision(tetraPos + wind, tetra, grid)
            tetraPos = if (!windCollides) {
                tetraPos + wind
            } else {
                tetraPos
            }

            val downCollides = checkCollision(tetraPos + Direction.DOWN, tetra, grid)
            if (downCollides) {
                val elements = tetra.map { it + tetraPos }
                grid.addAll(elements)

                val top = elements.maxOf { it.y }
                if (top > highest) {
                    val newWalls = (highest..top+5).flatMap { listOf(IntVec(0, it), IntVec(8, it)) }
                    grid.addAll(newWalls)
                    highest = top
                    grid.removeIf { it.y < highest-50}
                }


                break
            } else {
                tetraPos += Direction.DOWN
            }

        }


    }
    println("pattern")


    for (i in 1..heights.size/2) {
        val values = heights.chunked(i).map { it.first() }
        val diffs = values.zipWithNext { a,b -> b - a}
        val diff = diffs[1]
        if (diffs.drop(1).all { it == diff }) {
            println("pattern found!")
            println("multiplier = $i")
            println(values)
            println(diffs)
            println(diff)

            break
        }
    }

    // pattern found!
    // multiplier = 7
    // [0, 2127, 4247, 6367, 8487, 10607, 12727, 14847, 16967, 19087, 21207, 23327, 25447, 27567, 29687]
    // [2127, 2120, 2120, 2120, 2120, 2120, 2120, 2120, 2120, 2120, 2120, 2120, 2120, 2120]
    //
    // testcase multiplier =7, so 7*winds.size*tetraminos.size=7*200=1400
    // => [0, 2127, 4247, 6367, 8487, 10607, 12727, 14847, 16967
    // all with a diff of 2120 except first: [2127, 2120, 2120, 2120,
    // 1_000_000_000_000 / 1400 = 714_285_714,2857143, 714_285_714*1400 = 999_999_999_600
    // 714_285_715*1400 = 1_000_000_001_000
    // so doesn't divide cleanly, and ends up 1000 too high or 400 too low
    // since it repeats every 1400, can instead look at something matches
    // if (i % (winds.size * tetraminos.size*7) == 400) =>
    // [608, 2728, 4848, 6968, 9088,
    // with same diff, 2120 all the way so is repeating
    // is always 601 higher than the 0-based one
    // 714_285_713*2120+2127 = 1_514_285_713_687 after 400 too little
    // adding 601 => 1_514_285_714_288 which is correct!

    // REAL CASE
    // pattern found!
    // multiplier = 345
    // [0, 26831966, 53663935, 80495904, 107327873, 134159842]
    // [26831966, 26831969, 26831969, 26831969, 26831969]
    // 345*10_091*5=17_406_975
    // divided => 57_448, gives ending up at 4_100_200 too much
    // if (i % (winds.size * tetraminos.size*345) == 4_100_200) =>
    // [6320253, 33152222, 59984191] => 6_320_256 diff
    // 57_447*26_831_969+26_831_966 + 6_320_256 => 1541449275365


    val diff = heights.zipWithNext { a, b -> b - a }
    println(heights)
    println(diff)

    return highest
}


fun main() {

//    run("1", fileName = "day17_ex.txt", func = ::day17_1)
//    run("2", fileName = "day17_ex.txt", func = ::day17_2)

//    run("1", fileName = "day17.txt", func = ::day17_1)
    run("2", fileName = "day17.txt", func = ::day17_2)
}

/*
OUTPUT
======


 */