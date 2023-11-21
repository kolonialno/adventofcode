package com.matsemann.adventofcode2022

import com.matsemann.adventofcode2022.IntVec.Companion.showAsGrid


fun day10_1(lines: List<String>): Any {
    return lines.flatMap {
        if (it.startsWith("addx")) listOf("noop", it) else listOf(it)
    }.foldIndexed(1 to 0) { index, (register, strength), command ->
        val newRegister = if (command == "noop") {
            register
        } else {
            val adder = command.split(" ")[1].toInt()
            register + adder
        }

        val newStrength = if ((index + 21) % 40 == 0) {
            strength + register * (index + 1)
        } else strength

        newRegister to newStrength
    }.second
}

fun day10_2(lines: List<String>): Any {
    return "\n" + lines.flatMap {
        if (it.startsWith("addx")) listOf("noop", it) else listOf(it)
    }.foldIndexed<String, Pair<Int, Set<IntVec>>>(1 to setOf()) { index, (register, pixels), command ->
        val newRegister = if (command == "noop") {
            register
        } else {
            val adder = command.split(" ")[1].toInt()
            register + adder
        }

        val newPixels = if ((index + 1) % 40 - 1 in register - 1..register + 1) {
            pixels + IntVec((index) % 40, index / 40)
        } else pixels

        newRegister to newPixels
    }.second.showAsGrid()
}

fun main() {
//    run("1", fileName = "day10_ex.txt", func = ::day10_1)
//    run("2", fileName = "day10_ex.txt", func = ::day10_2)

    run("1", fileName = "day10.txt", func = ::day10_1)
    run("2", fileName = "day10.txt", func = ::day10_2)
}

/*
OUTPUT
======
Done. Took 29ms to run
Result for 1:	14320
Copied to clipboard!

Done. Took 15ms to run
Result for 2:
███   ██  ███  ███  █  █  ██  ███    ███
█  █ █  █ █  █ █  █ █ █  █  █ █  █    █
█  █ █    █  █ ███  ██   █  █ █  █    █
███  █    ███  █  █ █ █  ████ ███     ██
█    █  █ █    █  █ █ █  █  █ █    █  ██
█     ██  █    ███  █  █ █  █ █     ██


 */