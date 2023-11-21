package com.matsemann.adventofcode2022

import com.matsemann.adventofcode2022.utils.*
import java.util.PriorityQueue
import kotlin.math.abs

data class Vec3D(val x: Int, val y: Int, val z: Int)

fun Vec3D.neighbors() = listOf(
    Vec3D(x, y - 1, z), // below
    Vec3D(x, y + 1, z), // above
    Vec3D(x - 1, y, z), // left
    Vec3D(x + 1, y, z), // right
    Vec3D(x, y, z + 1), // forward
    Vec3D(x, y, z - 1), // backward
)

fun day18_1(lines: List<String>): Any {
    return lines
        .map { it.allInts() }
        .map { (x, y, z) -> Vec3D(x, y, z) }
        .fold((0 to setOf<Vec3D>())) { (sides, cubes), cube ->
            val count = cube.neighbors().count { it in cubes }
            val newSides = sides + 6 - 2 * count
            newSides to (cubes + cube)
        }.first
}


fun day18_2(lines: List<String>): Any {
    val allCubes = lines
        .map { it.allInts() }
        .map { (x, y, z) -> Vec3D(x, y, z) }

    val addedCubes = mutableSetOf<Vec3D>()
    val safeCube = Vec3D(allCubes.minOf { it.x } - 1, allCubes.minOf { it.y } - 1, allCubes.minOf { it.z } - 1)

    fun cubeIsReachable(cube: Vec3D): Pair<Boolean, Set<Vec3D>> {
        val toVisitQueue = PriorityQueue<Pair<Int, Vec3D>>(Comparator.comparing { it.first })
        val visited = mutableSetOf<Vec3D>()
        val gScores = DefaultMap2<Vec3D,_> { 99999}

        toVisitQueue.add(0 to cube)
        gScores[cube] = 0

        while (toVisitQueue.isNotEmpty()) {
            val (_, vec) = toVisitQueue.poll()
            visited.add(vec)

            if (vec == safeCube) {
                return true to visited
            }

            vec.neighbors()
                .filter { it !in visited }
                .filter { it !in addedCubes }
                .forEach { n ->
                    val newScore = gScores[vec] + 1
                    if (newScore < gScores[n]) {
                        gScores[n] = newScore
                        val fScore =
                            newScore.toInt() + abs(n.x - safeCube.x) + abs(n.y - safeCube.y) + abs(n.z - safeCube.z)
                        toVisitQueue.removeIf { it.second == n }
                        toVisitQueue.add(fScore to n)
                    }
                }
        }

        return false to visited
    }

    var score = 0
    for (cube in allCubes) {
        if (cube in addedCubes) {
            continue
        }

        val (existing, spaces) = cube.neighbors().partition { it in addedCubes }
        score += 6 - 2 * existing.count()
        addedCubes.add(cube)

        spaces.forEach { space ->
            if (space !in addedCubes) { // from previous space
                val (reachable, visited) = cubeIsReachable(space)
                if (!reachable) {
                    val toRemove = visited.map { v -> v.neighbors().count { it in addedCubes } }.sum()
                    score -= toRemove
                    addedCubes.addAll(visited)
                }
            }
        }

    }


    return score
}

fun main() {

//    run("1", fileName = "day18_ex.txt", func = ::day18_1)
//    run("2", fileName = "day18_ex.txt", func = ::day18_2)

//    run("1", fileName = "day18.txt", func = ::day18_1)
    run("2", fileName = "day18.txt", func = ::day18_2)
}

/*
OUTPUT
======


 */