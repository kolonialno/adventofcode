package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*

fun day15_1(lines: List<String>): Any {
    return lines.first().split(",").map { str ->
        str.fold(0) { hash, next -> ((hash + next.code) * 17) % 256 }
    }.sum()
}


fun day15_2(lines: List<String>): Any {
    val boxes = DefaultMap<Int, _> { mutableMapOf<String, Int>() } // map backed by a linked list

    lines.first().split(",").forEach { str ->
        val (label, lense) = str.split("=", "-")
        val hash = label.fold(0) { hash, next -> ((hash + next.code) * 17) % 256 }

        when (str.contains("-")) {
            true -> boxes[hash] -= label
            false -> boxes[hash][label] = lense.toInt()
        }
    }

    return boxes.map { (box, lenses) ->
        lenses.values.mapIndexed { pos, lense ->
            (pos + 1) * lense
        }.sum() * (box + 1)
    }.sum()
}
fun main() {

//    run("1", fileName = "day15_ex.txt", func = ::day15_1)
    run("2", fileName = "day15_ex.txt", func = ::day15_2)

    // 372776 too high??
//    run("1", fileName = "day15.txt", func = ::day15_1)
    run("2", fileName = "day15.txt", func = ::day15_2)
}
