package com.matsemann.adventofcode2021

import com.matsemann.adventofcode2021.IntVec.Companion.bounds

val nineGrid = (-1..1).flatMap { y -> (-1..1).map { x -> IntVec(x, y) } }

fun day20(lines: List<String>): Any {
    val substitution = lines.first().map { if (it == '.') 0 else 1 }

    var pixels = lines.drop(2).flatMapIndexed { y, line ->
        line.mapIndexed { x, c -> IntVec(x, y) to if (c == '.') 0 else 1 }
    }.toMap()

    var bounds = pixels.keys.bounds()
    var infValue = 0

    for (i in 0 until 50) {
        val pixelsToCalculate = pixels.keys.toMutableSet()

        // Add border pixels to the set
        bounds = listOf(bounds[0] - 1, bounds[1] + 1, bounds[2] - 1, bounds[3] + 1)
        for (x in bounds[0]..bounds[1]) {
            pixelsToCalculate += IntVec(x, bounds[2])
            pixelsToCalculate += IntVec(x, bounds[3])
        }
        for (y in (bounds[2])..(bounds[3])) {
            pixelsToCalculate += IntVec(bounds[0], y)
            pixelsToCalculate += IntVec(bounds[1], y)
        }

        pixels = pixelsToCalculate.associate { px ->
            nineGrid.map { it + px }
                .joinToString("") { pixels.getOrDefault(it, infValue).toString() }
                .let { px to substitution[it.toInt(2)] }
        }

        // Keep track of what the values outside the grid would be
        infValue = substitution[infValue.toString().repeat(9).toInt(2)]
    }

//    println(pixels.filter { (k, v) -> v == 1 }.keys.showAsGrid())
    return pixels.values.count { it == 1 }
}


fun main() {
//    run("1", fileName = "day20_ex.txt", func = ::day20)
    run("1", fileName = "day20_1.txt", func = ::day20)
}
