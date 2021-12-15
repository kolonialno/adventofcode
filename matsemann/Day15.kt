package com.matsemann.adventofcode2021

import com.matsemann.adventofcode2021.IntVec.Companion.bounds
import java.math.BigInteger
import java.util.*

fun day15_1(lines: List<String>): Any {

    val points = lines.flatMapIndexed { y, line ->
        line.split("").filterNot { it.isBlank() }
            .mapIndexed { x, cost -> IntVec(x, y) to cost.toInt() }
    }.toMap()

    val b = points.keys.toList().bounds()
    val bounds = IntVec(b[1], b[3])

    val visited = mutableSetOf<IntVec>()
    val costs = Counter<IntVec>()
    points.keys.forEach { costs[it] = Int.MAX_VALUE.toBigInteger() }
    costs[IntVec(0, 0)] = BigInteger.ZERO

    val queue = PriorityQueue<Pair<IntVec, BigInteger>>(Comparator.comparing { it.second })
    queue.offer(IntVec(0, 0) to BigInteger.ZERO)

    while (queue.isNotEmpty()) {
        val u = queue.poll()

        if (u.first == bounds) {
            return u.second
        }

        visited.add(u.first)


        u.first.neighbors(bounds).filterNot { it in visited }.forEach { n ->
            val newDist = costs[u.first] + points[n]!!
            if (newDist < costs[n]) {
                queue.removeIf { it.first == n }
                costs[n] = newDist
                queue.offer(n to newDist)
            }
        }
    }

    val smallest = points.keys.filter { it.y == bounds.y }
        .minByOrNull { costs[it] }

    return costs[smallest]!!


}

fun day15_2(lines: List<String>): Any {
    val bounds = IntVec(lines.first().length, lines.size)

    val points = lines.flatMapIndexed { y, line ->
        line.split("").filterNot { it.isBlank() }
            .flatMapIndexed { x, cost ->
                (0..4).flatMap { y1 ->
                    (0..4).map { x1 ->
                        val newCost = (cost.toInt() + y1 + x1)
                        val newNewCost = if (newCost >= 10) (newCost + 1) % 10 else newCost
                        IntVec(x + x1 * bounds.x, y1 * bounds.y + y) to newNewCost
                    }
                }
            }
    }.toMap()

    val gridBounds = points.keys.toList().bounds().let {
        IntVec(it[1], it[3])
    }

//    for (y in 0..gridBounds.y) {
//        for (x in 0..gridBounds.x) {
//            print(points[IntVec(x, y)])
//        }
//        println()
//    }

    val visited = mutableSetOf<IntVec>()
    val costs = Counter<IntVec>()
    points.keys.forEach { costs[it] = Int.MAX_VALUE.toBigInteger() }
    costs[IntVec(0, 0)] = BigInteger.ZERO

    val queue = PriorityQueue<Pair<IntVec, BigInteger>>(Comparator.comparing { it.second })
    queue.offer(IntVec(0, 0) to BigInteger.ZERO)

    while (queue.isNotEmpty()) {
        val u = queue.poll()
        visited.add(u.first)

        if (u.first == gridBounds) {
            return u.second
        }

        u.first.neighbors(gridBounds).filterNot { it in visited }.forEach { n ->
            val newDist = costs[u.first] + points[n]!!
            if (newDist < costs[n]) {
                queue.removeIf { it.first == n }
                costs[n] = newDist
                queue.offer(n to newDist)
            }
        }
    }

    return "not found"
}

fun main() {
//    run("1", fileName = "day15_ex.txt", func = ::day15_1)
        run("1", fileName = "day15_1.txt", func = ::day15_1)
//    run("2", fileName = "day15_ex.txt", func = ::day15_2)
        run("2", fileName = "day15_1.txt", func = ::day15_2)

}

/*
OUTPUT
======

Done. Took 23ms to run
Result for 1:	366
Copied to clipboard!

Done. Took 3620ms to run
Result for 2:	2829
Copied to clipboard!

 */