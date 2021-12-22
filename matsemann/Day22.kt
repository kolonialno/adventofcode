package com.matsemann.adventofcode2021

import java.math.BigInteger


data class Point3D(val x: Int, val y: Int, val z: Int)
data class Instruction(val on: Boolean, val xRange: IntRange, val yRange: IntRange, val zRange: IntRange)

fun day22_1(lines: List<String>): Any {

    fun pointsFor(instruction: Instruction) =
        instruction.xRange.flatMap { x ->
            instruction.yRange.flatMap { y ->
                instruction.zRange.map { z -> Point3D(x, y, z) }
            }
        }

    return lines.map { line ->
        line.allInts().let {
            Instruction(line.startsWith("on"), it[0]..it[1], it[2]..it[3], it[4]..it[5])
        }
    }.filterNot {
        listOf(it.xRange, it.yRange, it.zRange).any { range -> range.first < -50 || range.last > 50 }
    }.fold(setOf<Point3D>()) { acc, ins ->
        if (ins.on) {
            acc + pointsFor(ins).toSet()
        } else {
            acc - pointsFor(ins).toSet()
        }
    }.size

}


data class Cube(val point1: Point3D, val point2: Point3D) {
    private val subtractions = mutableListOf<Cube>()

    fun subtract(otherCube: Cube) {
        getIntersectingCube(this, otherCube)?.let { intersect ->
            subtractions.forEach { sub ->
                sub.subtract(intersect)
            }
            subtractions += intersect
        }
    }

    fun volume(): BigInteger {
        val myVolume = (point2.x - point1.x + 1).big() *
                (point2.y - point1.y + 1).big() *
                (point2.z - point1.z + 1).big()

        return myVolume - subtractions.sumOf { it.volume() }
    }

    companion object {
        fun getIntersectingCube(cube1: Cube, cube2: Cube): Cube? {
            val overlaps = (cube2.point1.x <= cube1.point2.x
                    && cube1.point1.x <= cube2.point2.x)
                    && (cube2.point1.y <= cube1.point2.y
                    && cube1.point1.y <= cube2.point2.y)
                    && (cube2.point1.z <= cube1.point2.z
                    && cube1.point1.z <= cube2.point2.z)
            if (!overlaps) {
                return null
            }

            val xRange = maxOf(cube1.point1.x, cube2.point1.x) to minOf(cube1.point2.x, cube2.point2.x)
            val yRange = maxOf(cube1.point1.y, cube2.point1.y) to minOf(cube1.point2.y, cube2.point2.y)
            val zRange = maxOf(cube1.point1.z, cube2.point1.z) to minOf(cube1.point2.z, cube2.point2.z)

            return Cube(
                Point3D(xRange.first, yRange.first, zRange.first),
                Point3D(xRange.second, yRange.second, zRange.second)
            )
        }
    }
}

fun day22_2(lines: List<String>): Any {

    val instructions = lines.map { line ->
        line.allInts().let {
            Instruction(line.startsWith("on"), it[0]..it[1], it[2]..it[3], it[4]..it[5])
        }
    }

    val cubes = mutableListOf<Cube>()

    for (ins in instructions) {
        val newCube = Cube(
            Point3D(ins.xRange.first, ins.yRange.first, ins.zRange.first),
            Point3D(ins.xRange.last, ins.yRange.last, ins.zRange.last),
        )
        cubes.forEach { cube ->
            cube.subtract(newCube)
        }
        if (ins.on) {
            cubes.add(newCube)
        }
    }

    return cubes.sumOf { it.volume() }
}

fun main() {
//    run("1", fileName = "day22_ex.txt", func = ::day22_1)
    run("1", fileName = "day22_1.txt", func = ::day22_1)
//    run("2", fileName = "day22_ex.txt", func = ::day22_2)
    run("2", fileName = "day22_1.txt", func = ::day22_2)
}

/*
OUTPUT
======

Done. Took 1447ms to run
Result for 1:	551693
Copied to clipboard!

Done. Took 11ms to run
Result for 2:	1165737675582132
Copied to clipboard!

 */