package com.matsemann.adventofcode2021

import java.lang.Integer.parseInt as parseInt

fun day03_1(lines: List<String>): Any {
    val counts = IntArray(lines[0].length)

    lines.forEach { word ->
        word.forEachIndexed { index, c ->
            if (c == '1') {
                counts[index]++
            }
        }
    }
    val half = lines.size / 2
    val gam = counts.joinToString("") { if (it > half) "1" else "0" }
        .let { parseInt(it, 2) }
    val eps = counts.joinToString("") { if (it > half) "0" else "1" }
        .let { parseInt(it, 2) }

    return gam * eps
}

fun lifeSupport(lines: List<String>, common: Char, leastCommon: Char) =
    (0 until lines[0].length)
        .fold(lines) { acc, index ->
            val count = acc.count { word ->
                word[index] == '1'
            }
            val half = acc.size / 2f

            val toKeep = if (count >= half) common else leastCommon

            if (acc.size != 1)
                acc.filter {
                    it[index] == toKeep
                }
            else
                acc
        }.let { parseInt(it.first(), 2) }


fun day03_2(lines: List<String>): Any {
    val ox = lifeSupport(lines, '1', '0')
    val co2 = lifeSupport(lines, '0', '1')
    return ox * co2
}

fun main() {
//    run("1", fileName = "day03_ex.txt", func = ::day03_1)
    run("1", fileName = "day03_1.txt", func = ::day03_1)
//    run("2", fileName = "day03_ex.txt", func = ::day03_2)
    run("2", fileName = "day03_1.txt", func = ::day03_2)
}

/*
OUTPUT
======

Done. Took 3ms to run
Result for 1:	2954600
Copied to clipboard!

Done. Took 38ms to run
Result for 2:	1662846
Copied to clipboard!

 */