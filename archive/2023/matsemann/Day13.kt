package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*
import kotlin.math.min

fun day13_1(lines: List<String>): Any {
    fun findPattern(pattern: List<String>) : Int? {
        for (counter in 0..<pattern.size-1) {
            var valid = true
            val steps = min(counter,pattern.size - (counter + 2))
            for (i in 0..steps) {
                val left = counter - i
                val right = counter + 1 + i
                if (pattern[left] != pattern[right]) {
                    valid = false
                    break
                }
            }
            if (valid) {
                return counter + 1
            }
        }
        return null
    }

    return lines.splitBy { it == "" }.map { pattern ->
        val horisontal = findPattern(pattern)
        if (horisontal != null) {
            horisontal * 100
        } else {
            findPattern(pattern.transpose())!!
        }
    }.println().sum()
}


fun day13_2(lines: List<String>): Any {

    fun findPattern(pattern: List<String>) : Int? {
        for (counter in 0..<pattern.size-1) {
            var smudgeUsed = false
            var valid = true

            val steps = min(counter,pattern.size - (counter + 2))
            for (i in 0..steps) {
                val left = counter - i
                val right = counter + 1 + i

                val diffs = pattern[left].zip(pattern[right]).count { (c1, c2) -> c1 != c2 }
                if (!smudgeUsed && diffs == 1) {
                    smudgeUsed = true
                } else if (diffs != 0) {
                    valid = false
                    break
                }
            }
            if (valid && smudgeUsed) {
                return counter + 1
            }
        }
        return null
    }

    return lines.splitBy { it == "" }.map { pattern ->
        val horisontal = findPattern(pattern)
        if (horisontal != null) {
            horisontal * 100
        } else {
            findPattern(pattern.transpose())!!
        }
    }.println().sum()
}

fun main() {

//    run("1", fileName = "day13_ex.txt", func = ::day13_1)
    run("2", fileName = "day13_ex.txt", func = ::day13_2)


//    run("1", fileName = "day13.txt", func = ::day13_1)
    run("2", fileName = "day13.txt", func = ::day13_2)
}
