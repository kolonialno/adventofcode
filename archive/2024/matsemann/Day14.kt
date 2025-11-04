package com.matsemann.adventofcode2024

import com.matsemann.adventofcode2024.utils.*
import com.matsemann.adventofcode2024.utils.IntVec.Companion.showAsGrid


fun day14_1(lines: List<String>): Any {
    val robots = lines.map { line ->
        line.allInts().let {
            IntVec(it[0], it[1]) to IntVec(it[2], it[3])
        }
    }

    val maxX = robots.maxOf { (pos) -> pos.x }
    val maxY = robots.maxOf { (pos) -> pos.y }

    return robots.map { (pos, vel) ->
        pos + vel * 100
    }.map { pos ->
        IntVec(pos.x.mod(maxX + 1), pos.y.mod(maxY + 1))
    }.filter { pos ->
        pos.x != maxX / 2 && pos.y != maxY / 2
    }.groupBy { pos ->
        (pos.x < (maxX / 2)) to (pos.y < (maxY / 2))
    }.values.fold(1) { acc, group -> acc * group.size }
}


fun day14_2(lines: List<String>): Any {
    // ???????????????
    // when they're all close to someone else perhaps?

    val robots = lines.map { line ->
        line.allInts().let {
            IntVec(it[0], it[1]) to IntVec(it[2], it[3])
        }
    }

    val maxX = robots.maxOf { (pos) -> pos.x }
    val maxY = robots.maxOf { (pos) -> pos.y }


    val pictures = infiniteSequence().runningFold(robots) {prevRobots, _ ->
        prevRobots.map { (pos, vel) -> pos + vel to vel }.map { (pos, vel) ->
            IntVec(pos.x.mod(maxX+1), pos.y.mod(maxY+1)) to vel
        }
    }.take(12000).toList()
// Well gave up looking for it hehe
//        .forEachIndexed { i, it ->
//        println(i)
//        println(it.map { it.first }.showAsGrid())
//        Thread.sleep(30)
//    }

    // When they all have a close neighbor, so they all paint a picture together?
    // Didn't owrk
//    pictures.sortedBy { pic ->
//        pic.sumOf { me -> pic.minOf { other -> other.first.chebyshev(me.first) } }
//    }.forEach {
//        println(it.map { it.first }.showAsGrid())
//        Thread.sleep(200)
//    }

    // When maximum are in the same row/col?
    // Works!
//    pictures.withIndex().sortedByDescending { (i, pic) ->
//        pic.map { it.first }.groupBy { it.x }.maxOf { it.value.size } * pic.map { it.first }.groupBy { it.y }.maxOf { it.value.size }
//    }.first().let { (i, pic) ->
//        println(i)
//        println(pic.map { it.first }.showAsGrid())
//    }

    // Ah, but also works just finding the one with the lowest value from part1
    pictures.withIndex().sortedBy { (i, pic) ->
        pic.map { it.first }.filter { pos ->
            pos.x != maxX / 2 && pos.y != maxY / 2
        }.groupBy { pos ->
            (pos.x < (maxX / 2)) to (pos.y < (maxY / 2))
        }.values.fold(1) { acc, group -> acc * group.size }
    }.first().let { (i, pic) ->
        println(i)
        println(pic.map { it.first }.showAsGrid())
    }
    return "wtf"
}

fun main() {

//    run("1", fileName = "day14_ex.txt", func = ::day14_1)
//    run("2", fileName = "day14_ex.txt", func = ::day14_2)


//    run("1", fileName = "day14.txt", func = ::day14_1)
    run("2", fileName = "day14.txt", func = ::day14_2)
}
