package com.matsemann.adventofcode2024

import com.matsemann.adventofcode2024.utils.*

/**
 * 94a + 22b = 8400
 * 34a + 67b = 5400
 * min: 3*a + b
 *
 *  b = (8400 - 94a) / 22
 *  b = (R0 -A0*a) / B0
 *
 *  34a + 67((8400 - 94a) / 22)) = 5400
 *  34a + 25581,82 - 286,27a = 5400
 *  -252,27a = -20181
 *  a = 80
 *
 *
 *  b = (R0 -A0*a) / B0
 *  A1*a + B1 * ((R0 -A0*a) / B0)) = R1
 *  A1*a + (B1 * R0 / B0) - (B1 * A0*a / B0) = R1
 *  A1*a - (B1 * A0*a / B0) = R1 - (B1 * R0 / B0)
 *  B0*A1*a - B1*A0*a = B0 * R1 - B1 * R0
 *  (B0*A1 - B1*A0)*a =
 *
 *  a = (B0 * R1 - B1 * R0)
 *      / (B0*A1 - B1*A0)
 *
 *  a = (22*5400 - 67*8400) / (22*34 - 67*94)
 *  a = (118800 - 562800) / (748 - 6298) = -444 000 / - 5550 = 80
 *
 */


fun day13_1(lines: List<String>): Any {
    return lines.splitBy { it == "" }.map { threeLines ->
        threeLines.map { it.allInts().map { it.toLong() } }
    }.map { (a, b, prize) ->
        val numAs = (b[0]*prize[1] - b[1]*prize[0]) / (b[0]*a[1] - b[1]*a[0])

        val numBs = (prize[0] - a[0] * numAs) / b[0]

        if (prize[0] == a[0]*numAs + b[0] * numBs &&
            prize[1] == a[1]*numAs + b[1] * numBs) {
            3 * numAs + numBs
        } else {
            0
        }
    }.sum()
}


fun day13_2(lines: List<String>): Any {
    return lines.splitBy { it == "" }.map { threeLines ->
        threeLines.map { it.allInts().map { it.toLong() } }
    }.map { (a, b, originalPrize) ->
        val prize = listOf(originalPrize[0] + 10000000000000L, originalPrize[1] + 10000000000000L)

        val numAs = (b[0] * prize[1] - b[1] * prize[0]) / (b[0] * a[1] - b[1] * a[0])

        val numBs = (prize[0] - a[0] * numAs) / b[0]

        if (prize[0] == a[0] * numAs + b[0] * numBs &&
            prize[1] == a[1] * numAs + b[1] * numBs
        ) {
            3 * numAs + numBs
        } else {
            0
        }
    }.sum()
}

fun main() {

    run("1", fileName = "day13_ex.txt", func = ::day13_1)
//    run("2", fileName = "day13_ex.txt", func = ::day13_2)


    run("1", fileName = "day13.txt", func = ::day13_1)
    run("2", fileName = "day13.txt", func = ::day13_2)
}
