package com.matsemann.adventofcode2022

import com.matsemann.adventofcode2022.utils.*
import kotlin.math.abs

fun day15_1(lines: List<String>): Any {
//    val goal = 10
    val goal = 2000000
    val sensors = lines.map { it.allInts() }
        .map { (i1, i2, i3, i4) ->
            IntVec(i1, i2) to IntVec(i3, i4)
        }

    val safePointsRow10 = mutableSetOf<Int>()

    sensors.forEach { (sensor, beacon) ->
        val dst = sensor.manhattan(beacon)
        val dstTo10 = abs(sensor.y - goal)
        // Then can go dstTo10-dst to both sides
        val width = dst - dstTo10

        (sensor.x - width..sensor.x + width).forEach { x ->
            val pos = IntVec(x, goal)
            if (pos != beacon) {
                safePointsRow10.add(x)
            }
        }

    }

    return safePointsRow10.size
}


fun day15_2(lines: List<String>): Any {
//    val max = 20
    val max = 4000000

    val sensors = lines.map { it.allInts() }
        .map { (i1, i2, i3, i4) ->
            IntVec(i1, i2) to IntVec(i3, i4)
        }


    val safeRangesPerLine = Array<MutableList<IntRange>>(max + 1) { mutableListOf() }


    sensors.forEach { (sensor, beacon) ->
        for (y in 0..max) {
            val dst = sensor.manhattan(beacon)
            val dstToY = abs(sensor.y - y)
            val width = dst - dstToY

            if (width > 0) {
                // Add a start/end of safe spot indexes
                safeRangesPerLine[y].add(sensor.x - width..sensor.x + width)
            }

        }

    }

    safeRangesPerLine.forEachIndexed { y, ranges ->
        val sortedRanges = ranges.sortedBy { it.first }
        var highest = sortedRanges.first().last

        // Find first set of ranges with a gap
        sortedRanges.drop(1).map {
            if (it.first > highest) {
                return (it.first - 1).big() * 4000000.big() + y.big()
            }
            if (it.last > highest) {
                highest = it.last
            }
        }
    }


    /** naive too slow */
//    for (y in 0..max) {
//
//        for (x in 0..max) {
//            var foundX = false
//            for (set in safeRangesPerLine[y]) {
//                if (x in set) {
//                    foundX = true
//                    break
//                }
//            }
//            if (!foundX) {
//                return x * 4000000 + y
//            }
//        }
//    }

    return "wtf"
}

fun main() {

//    run("1", fileName = "day15_ex.txt", func = ::day15_1)
//    run("2", fileName = "day15_ex.txt", func = ::day15_2)

//    run("1", fileName = "day15.txt", func = ::day15_1)
    run("2", fileName = "day15.txt", func = ::day15_2)
}

/*
OUTPUT
======

 */