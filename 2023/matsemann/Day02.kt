package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*
import kotlin.math.max

data class CubeDraw(val r: Int, val g: Int, val b: Int)

fun parseCubeDraws(lines: List<String>): List<Pair<Int, List<CubeDraw>>> {
    return lines.map {
        val spl = it.split(":")
        val gameNr = spl[0].firstInt()

        val draws = spl[1].split("; ").map { draw ->
            val colors = draw.split(", ")
            val r = colors.find { color -> color.contains("red") }?.firstInt() ?: 0
            val g = colors.find { color -> color.contains("green") }?.firstInt() ?: 0
            val b = colors.find { color -> color.contains("blue") }?.firstInt() ?: 0
            CubeDraw(r, g, b)
        }
        gameNr to draws
    }
}

fun day02_1(lines: List<String>): Any {
    val limit = CubeDraw(12, 13, 14)

    return parseCubeDraws(lines)
        .filter {
            it.second.all { draw ->
                draw.r <= limit.r && draw.g <= limit.g && draw.b <= limit.b
            }
        }.sumOf { it.first }
}


fun day02_2(lines: List<String>): Any {
    return parseCubeDraws(lines)
        .sumOf { game ->
            game.second.fold(CubeDraw(0, 0, 0)) { limit, draw ->
                CubeDraw(max(limit.r, draw.r), max(limit.g, draw.g), max(limit.b, draw.b))
            }.let { it.r * it.b * it.g }
        }
}

fun main() {

//    run("1", fileName = "day02_ex.txt", func = ::day02_1)
    run("2", fileName = "day02_ex.txt", func = ::day02_2)


//    run("1", fileName = "day02.txt", func = ::day02_1)
    run("2", fileName = "day02.txt", func = ::day02_2)
}
