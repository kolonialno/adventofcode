package com.matsemann.adventofcode2025

import com.matsemann.adventofcode2025.utils.*
import kotlin.math.abs


fun day01_1(lines: List<String>): Any {
    return lines.map {
        it.drop(1).toInt() * if (it[0] == 'R') -1 else 1
    }.fold(50 to 0) { (pos, tot), rot ->
        val newPos = pos + rot
        newPos to (tot + if (newPos % 100 == 0) 1 else 0)
    }.second
}


fun day01_2(lines: List<String>): Any {
    return lines.map {
        it.drop(1).toInt() * if (it[0] == 'R') -1 else 1
    }.fold(50 to 0) { (pos, tot), rot ->
        val newPos = pos + rot
        val flips = abs(pos.floorDiv(100) - newPos.floorDiv(100))
        // Feels not very elegant, but special handling when I land on an exact 100,
        // for instance from 201 to 200 the hundreth doesn't flip, so add 1
        val stoppedAt0up = if (newPos % 100 == 0 && newPos < pos) 1 else 0
        // or when I came from a whole hundreth, 200 to 199 counts a flip above
        // which we must remove
        val stoppedAt0down = if (pos % 100 == 0 && newPos < pos) -1 else 0
        val change = flips + stoppedAt0up + stoppedAt0down
        newPos to (tot + change)
    }.second
}

fun main() {

//    run("1", fileName = "day01_ex.txt", func = ::day01_1)
    run("2", fileName = "day01_ex.txt", func = ::day01_2)


//    run("1", fileName = "day01.txt", func = ::day01_1)
    run("2", fileName = "day01.txt", func = ::day01_2)
}
