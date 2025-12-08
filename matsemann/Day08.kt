package com.matsemann.adventofcode2025

import com.matsemann.adventofcode2025.utils.*
import kotlin.math.sqrt

data class Vec3d(val x: Double, val y: Double, val z: Double) {
    fun dst(other: Vec3d) =
        sqrt((x - other.x) * (x - other.x) + (y - other.y) * (y - other.y) + (z - other.z) * (z - other.z))
}

fun day08_1(lines: List<String>): Any {
    val junctions = lines.map { line ->
        line.allInts().let { Vec3d(it[0].toDouble(), it[1].toDouble(), it[2].toDouble()) }
    }
    // horrible, but at least without mutations...
    return combinations(junctions, 2)
        .map { (j1, j2) ->
            (j1 to j2) to j1.dst(j2)
        }.sortedBy { it.second }
        .take(1000)
        .fold(junctions.associateWith { setOf(it) }) { circuits, (pair, _) ->
            val (circ1, circ2) = circuits[pair.first]!! to circuits[pair.second]!!
            if (circ1 != circ2) {
                val newCirc = circ1 + circ2
                circuits.mapValues { (k, v) -> if (k in newCirc) newCirc else v }
            } else {
                circuits
            }
        }.values.distinct()
        .map { it.size }
        .sortedDescending()
        .take(3)
        .reduce { acc, i -> i * acc }

}


fun day08_2(lines: List<String>): Any {
    val junctions = lines.map { line ->
        line.allInts().let { Vec3d(it[0].toDouble(), it[1].toDouble(), it[2].toDouble()) }
    }
    // horrible, but at least without mutations...
    return combinations(junctions, 2)
        .map { (j1, j2) ->
            (j1 to j2) to j1.dst(j2)
        }.sortedBy { it.second }
        .runningFold(junctions.associateWith { setOf(it) } to null as Pair<Vec3d, Vec3d>?) { (circuits), (pair, _) ->
            val (circ1, circ2) = circuits[pair.first]!! to circuits[pair.second]!!
            if (circ1 != circ2) {
                val newCirc = circ1 + circ2
                circuits.mapValues { (k, v) -> if (k in newCirc) newCirc else v } to pair
            } else {
                circuits to pair
            }
        // run until they're all in the same circuit and take the last pairing used
        }.first { (circuits, _) -> circuits.values.distinct().size == 1 }
        .second.let { it!!.first.x.toLong() * it.second.x.toLong() }
}

fun main() {

//    run("1", fileName = "day08_ex.txt", func = ::day08_1)
    run("2", fileName = "day08_ex.txt", func = ::day08_2)


//    run("1", fileName = "day08.txt", func = ::day08_1)
    run("2", fileName = "day08.txt", func = ::day08_2)
}
