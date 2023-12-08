package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*

fun day08_1(lines: List<String>): Any {
    val instructions = lines.first().toList()

    val nodes = lines.drop(2).associate { line ->
        val parts = line.split(" = (")
        val parts2 = parts[1].split(", ")
        parts[0] to (parts2[0] to parts2[1].take(3))
    }

    val res = generateSequence(0L) { it + 1 }.runningFold("AAA") { curr, step ->
        val dir = instructions.circular(step)
        val node = nodes[curr]!!
        if (dir == 'L')node.first else node.second
    }.withIndex().first { it.value == "ZZZ" }

    return res.index
}

fun day08_22(lines: List<String>): Any {
    val instructions = lines.first().toList()

    val nodes = lines.drop(2).associate { line ->
        val parts = line.split(" = (")
        val parts2 = parts[1].split(", ")
        parts[0] to (parts2[0] to parts2[1].take(3))
    }

    val start = nodes.keys.filter { it.endsWith("A") }


    val cycles = start.map {startPos ->
        generateSequence(0) { it + 1 }.runningFold(listOf(Triple(startPos, 0,0))) { curr, step ->
            val instructionPos = step.mod(instructions.size)
            val dir = instructions[instructionPos]
            val node = nodes[curr.last().first]!!
            val next = if (dir == 'L') node.first else node.second
            curr + Triple(next, instructionPos + 1, step + 1)
        }.withIndex()
            .first { (it.value.last().first to it.value.last().second) in it.value.dropLast(1).map { it.first to it.second } }
    }

    cycles.map {
        val last = it.value.last()

        val start = it.value.first { it.first == last.first && it.second == last.second }

        val index = start.third
        val length = last.third - index

        val Zs = it.value.drop(index).take(length).filter { it.first.endsWith("Z")}.map { it.third }
        listOf(start, last, Zs)
    }.println()
    // Then manually inspect the cycles, find the period of the XXZ inside it, and use a LCM calculator
    // to find where all the cycles align

    return "manual"

}

fun main() {

//    run("1", fileName = "day08_ex.txt", func = ::day08_1)
    run("2", fileName = "day08_ex.txt", func = ::day08_22)


//    run("1", fileName = "day08.txt", func = ::day08_1)
    run("2", fileName = "day08.txt", func = ::day08_22)
}
