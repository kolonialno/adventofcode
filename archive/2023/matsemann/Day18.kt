package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*
import com.matsemann.adventofcode2023.utils.IntVec.Companion.allBetween
import com.matsemann.adventofcode2023.utils.IntVec.Companion.showAsGrid
import kotlin.math.absoluteValue

fun day18_1(lines: List<String>): Any {
    val vertices = lines.map { line ->
        line.split(" ").let {
            Direction.fromUDLR(it[0]) to it[1].toInt()
        }
    }.runningFold(IntVec.zero) { prev, (dir, value) ->
        prev + (dir.toVec() * value)
    }

    val edgePoints = vertices.zipWithNext { a, b ->
        a.allBetween(b).dropLast(1)
    }.flatten()

    val shoelace = vertices.zipWithNext { (x1, y1), (x2, y2) ->
        x1*y2 - x2*y1
    }.sum().let { it.absoluteValue / 2 }

    // Pick's theorem
    // area = interior_points + (boundary_points / 2) - 1
    // 42 = i + (38 / 2 ) - 1
    // i = 42 - 19 + 1 = 24
    // including boundaries = 24 + 38 = 62
    // so: area - (boundary/2) + 1 + boundary
    return shoelace + (edgePoints.size / 2) + 1
}


fun day18_2(lines: List<String>): Any {
    val vertices = lines.map { line ->
        line.split(" ")[2].let {
            when (it.drop(7).first()) {
                '0' -> Direction.RIGHT
                '1' -> Direction.DOWN
                '2' -> Direction.LEFT
                else -> Direction.UP
            } to it.drop(2).take(5).toInt(16)
        }
    }.println().runningFold(IntVec.zero) { prev, (dir, value) ->
        prev + (dir.toVec() * value)
    }

    val edgePoints = vertices.zipWithNext { a, b ->
        a.allBetween(b).dropLast(1)
    }.flatten()

    val shoelace = vertices.zipWithNext { (x1, y1), (x2, y2) ->
        (x1.toLong()*y2.toLong() - x2.toLong()*y1.toLong())
    }.sum().let { it.absoluteValue / 2 }

    return shoelace + (edgePoints.size / 2) + 1
}

fun main() {
    Direction.setYDown()
//    run("1", fileName = "day18_ex.txt", func = ::day18_1)
    run("2", fileName = "day18_ex.txt", func = ::day18_2)


//    run("1", fileName = "day18.txt", func = ::day18_1)
    run("2", fileName = "day18.txt", func = ::day18_2)
}
