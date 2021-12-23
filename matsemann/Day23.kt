package com.matsemann.adventofcode2021

import java.util.*
import kotlin.math.abs

val hallwayStops = listOf(0, 1, 3, 5, 7, 9, 10)
val doors = listOf(2, 4, 6, 8)
val room1 = listOf(11, 12, 13, 14)
val room2 = listOf(21, 22, 23, 24)
val room3 = listOf(31, 32, 33, 34)
val room4 = listOf(41, 42, 43, 44)
val rooms = listOf(room1, room2, room3, room4)
val costs = listOf(1, 10, 100, 1000)

data class State(val numNumbers: Int, val positions: List<Int>) {

    fun isFinished(): Boolean {
        return positions.chunked(numNumbers).mapIndexed { i, poses ->
            poses.all { it in rooms[i] }
        }.all { it }
    }

    fun getPossibleMoves(): List<Pair<Int, State>> {
        return positions.indices.flatMap { getMoves(it) }
    }

    private fun getMoves(amphi: Int): List<Pair<Int, State>> {
        val pos = positions[amphi]
        val goalRoom = amphi / numNumbers
        val goalRoomSpots = rooms[goalRoom]

        if (pos > 10) { // in a room
            if (pos in goalRoomSpots) { // don't make a move if in the correct spot
                // and nothing behind me that should out
                val range = (pos + 1)..((goalRoom + 1) * 10) + numNumbers
                val blockedBehind = positions.filterIndexed { i, p -> p in range && i / numNumbers != goalRoom }
                if (blockedBehind.isEmpty()) {
                    return listOf()
                }
            }

            val potentialBlockages = (pos - (pos % 10) + 1) until pos
            val isBlocked = positions.any { it in potentialBlockages }
            if (isBlocked) { // Can't move out
                return listOf()
            }


            val currentRoom = (pos / 10) - 1
            val entrance = doors[currentRoom]
            val stepToEntrance = pos % 10
            val cost = costs[goalRoom]

            // move to all possible hallway spots
            val moves = hallwayStops.mapNotNull { stop ->
                if (canGoTo(to = stop, from = entrance)) {
                    val dst = abs(entrance - stop)
                    val totalCost = cost * (stepToEntrance + dst)
                    val newState = positions.toMutableList()
                    newState[amphi] = stop
                    totalCost to State(numNumbers, newState)
                } else {
                    null
                }
            }
            return moves
        } else { // outside
            val doorPos = doors[goalRoom]
            val canGoToDoor = canGoTo(pos, doorPos)
            if (!canGoToDoor) {
                return listOf()
            }

            val anyoneNotSupposedInTheRoom = positions.filterIndexed { i, amphiPos ->
                amphiPos in goalRoomSpots && i / numNumbers != goalRoom
            }
            if (anyoneNotSupposedInTheRoom.any()) { // Too early to go in
                return listOf()
            }

            val numAlreadyThere = positions.count { it in goalRoomSpots }

            val stepsFromEntrance = numNumbers - numAlreadyThere
            val stepsToEntrance = abs(pos - doorPos)
            val finalPos = goalRoomSpots[numNumbers - numAlreadyThere - 1]
            val cost = costs[goalRoom]

            val totalCost = cost * (stepsToEntrance + stepsFromEntrance)

            val newState = positions.toMutableList()
            newState[amphi] = finalPos
            return listOf(totalCost to State(numNumbers, newState))
        }
    }

    private fun canGoTo(from: Int, to: Int): Boolean {
        val range = minOf(from + 1, to)..maxOf(from - 1, to)
        return !positions.any { pos -> pos in range }
    }
}

fun day23_1(lines: List<String>): Any {
    // Ex part1
//    val startState = listOf(room1[1], room4[1], room1[0], room3[0], room2[0], room3[1], room2[1], room4[0])
//    val startState = listOf(room1[1], room4[1], room1[0], 3, room2[0], room3[1], room2[1], room4[0])
//    val start = State(2, startState)

    // Part 1
//    val startState = listOf(room2[1], room4[0], room1[0], room2[0], room1[1], room4[1], room3[0], room3[1])
//    val start = State(2, startState)

    // Ex Part 2
//    val startState = listOf(
//        room1[3], room3[2], room4[1], room4[3],
//        room1[0], room2[2], room3[0], room3[1],
//        room2[0], room2[1], room3[3], room4[2],
//        room1[1], room1[2], room2[3], room4[0]
//    )
//    val start = State(4, startState)

    // Part 2
    val startState = listOf(
        room2[3], room3[2], room4[0], room4[1],
        room1[0], room2[0], room2[2], room3[1],
        room1[3], room2[1], room4[2], room4[3],
        room1[1], room1[2], room3[0], room3[3],
    )
    val start = State(4, startState)


    val visited = mutableSetOf<State>()
    val queue = PriorityQueue<Pair<Int, State>>(Comparator.comparing { it.first })
    queue.offer(0 to start)

    var count = 0

    while (queue.isNotEmpty()) {
        count++
        val u = queue.poll()

        if (u.second.isFinished()) {
            println("Searched: $count")
            return u.first
        }
        if (u.second in visited) {
            continue
        }
        visited.add(u.second)

        u.second.getPossibleMoves().filterNot { it.second in visited }.forEach { (cost, state) ->
            val totalCost = u.first + cost
            queue.offer(totalCost to state)
        }

    }
    return "failed"

}


fun main() {
    run("1", fileName = "day23_dummy.txt", func = ::day23_1)
}

/*
OUTPUT
======

Done. Took 2134ms to run
Result for 1:	11608
Copied to clipboard!


Searched: 467459
Done. Took 5960ms to run
Result for 1:	46754
Copied to clipboard!

 */