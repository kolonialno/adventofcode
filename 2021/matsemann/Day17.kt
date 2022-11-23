package com.matsemann.adventofcode2021

import kotlin.math.max
import kotlin.math.sign


fun day17_1(lines: List<String>): Any {
    return hits(lines.first())
        .maxOf { it.second }
}

fun day17_2(lines: List<String>): Any {
    return hits(lines.first())
        .count()
}

fun hits(line: String): List<Pair<Boolean, Int>> {
    val goal = line.allInts().let {
        Rectangle.fromNumbers(it[0], it[2], it[1], it[3])
    }

    val xRange = if (goal.pos1.x < 0) {
        goal.pos1.x..(max(goal.pos2.x, 0))
    } else {
        0..(goal.pos2.x)
    }
    val yRange = -150..150 // 🤷

    return xRange
        .flatMap { x ->
            yRange.map { y -> trajectory(IntVec(x, y), goal) }
        }
        .filter { it.first }
}

fun trajectory(startVel: IntVec, goal: Rectangle): Pair<Boolean, Int> {
    var vel = startVel
    var pos = IntVec(0, 0)

    var highestY = pos.y

    while (true) {
        val velChange = IntVec(-vel.x.sign, -1)

        pos += vel
        vel += velChange

        if (pos.y > highestY) {
            highestY = pos.y
        }

        if (pos in goal) {
            return true to highestY
        }

        if ((vel.y < 0 && goal.pointIsBelow(pos.y))
            || (goal.pos1.x > 0 && goal.pointIsRightOf(pos.x))
            || (goal.pos1.x < 0 && goal.pointIsLeftOf(pos.x))
        ) {
            break
        }
    }
    return false to 0
}


fun main() {

//    run("1", fileName = "day17_ex.txt", func = ::day17_1)
    run("1", fileName = "day17_1.txt", func = ::day17_1)
//    run("2", fileName = "day17_ex.txt", func = ::day17_2)
    run("2", fileName = "day17_1.txt", func = ::day17_2)
}

/*
OUTPUT
======

Done. Took 8ms to run
Result for 1:	9180
Copied to clipboard!

Done. Took 9ms to run
Result for 2:	3767
Copied to clipboard!

 */