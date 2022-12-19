package com.matsemann.adventofcode2022

import com.matsemann.adventofcode2022.utils.*
import kotlin.math.*

data class BluePrint(
    val id: Int,
    val oreOreNeeded: Int,
    val clayOreNeeded: Int,
    val obsidianOreNeeded: Int,
    val obsidianClayNeeded: Int,
    val geodeOreNeeded: Int,
    val geodeObsidianNeeded: Int
)

data class MineralState(
    val minute: Int,
    val ores: Int,
    val clays: Int,
    val obsidians: Int,
    val geodes: Int,
    val oreProduction: Int,
    val clayProduction: Int,
    val obsidianProduction: Int,
    val geodeProduction: Int,
)

fun progressState(state: MineralState, minutes: Int): MineralState {
    val iterations = min(maxMinutes - state.minute, minutes) // Stop at max rounds
    return state.copy(
        minute = state.minute + iterations,
        ores = state.ores + state.oreProduction * iterations,
        clays = state.clays + state.clayProduction * iterations,
        obsidians = state.obsidians + state.obsidianProduction * iterations,
        geodes = state.geodes + state.geodeProduction * iterations
    )
}


var maxMinutes = 24
var highestSoFar = 0
var prunedCount = 0

fun searchOres(state: MineralState, print: BluePrint): Int {
    /* Instead of simulating each timestep and doing a search in that,
     * I instead do a search in possible actions and "wait" until I can do it.
     */
    if (state.minute >= maxMinutes) {
        highestSoFar = max(state.geodes, highestSoFar)
        return state.geodes
    }

    // Max I can produce remaining rounds if I increase production 1 each round:
    val roundsLeft = maxMinutes - state.minute
    val canProduce = state.geodes + (0 until roundsLeft).sumOf { state.geodeProduction + it }
    if (canProduce < highestSoFar) {
        // Give up this tree early, will never beat existing path
        prunedCount++
        return 0
    }

    var max = 0

    // Wait & build geode robot
    if (state.obsidianProduction > 0) {
        val oresNeeded = max(0, print.geodeOreNeeded - state.ores)
        val obsidiansNeeded = max(0, print.geodeObsidianNeeded - state.obsidians)
        val roundsToWait = if (oresNeeded == 0 && obsidiansNeeded == 0) {
            1 // can build it next round
        } else {
            max(
                ceil(oresNeeded.toFloat() / state.oreProduction.toFloat()).toInt(),
                ceil(obsidiansNeeded.toFloat() / state.obsidianProduction.toFloat()).toInt(),
            ) + 1
        }
        val progressedState = progressState(state, roundsToWait)
        val stateAfterBuilt = progressedState.copy(
            ores = progressedState.ores - print.geodeOreNeeded,
            obsidians = progressedState.obsidians - print.geodeObsidianNeeded,
            geodeProduction = progressedState.geodeProduction + 1
        )

        val result = searchOres(stateAfterBuilt, print)
        max = max(result, max)
    }

    // Wait & build obsidian robot
    if (state.clayProduction > 0) {
        val oresNeeded = max(0, print.obsidianOreNeeded - state.ores)
        val claysNeeded = max(0, print.obsidianClayNeeded - state.clays)
        val roundsToWait = if (oresNeeded == 0 && claysNeeded == 0) {
            1
        } else {
            max(
                ceil(oresNeeded.toFloat() / state.oreProduction.toFloat()).toInt(),
                ceil(claysNeeded.toFloat() / state.clayProduction.toFloat()).toInt(),
            ) + 1
        }
        val progressedState = progressState(state, roundsToWait)
        val stateAfterBuilt = progressedState.copy(
            ores = progressedState.ores - print.obsidianOreNeeded,
            clays = progressedState.clays - print.obsidianClayNeeded,
            obsidianProduction = progressedState.obsidianProduction + 1
        )

        val result = searchOres(stateAfterBuilt, print)
        max = max(result, max)
    }

    // Wait & build clay robot
    if (state.oreProduction > 0) {
        val oresNeeded = max(0, print.clayOreNeeded - state.ores)
        val roundsToWait = if (oresNeeded == 0) {
            1
        } else {
            ceil(oresNeeded.toFloat() / state.oreProduction.toFloat()).toInt() + 1
        }
        val progressedState = progressState(state, roundsToWait)
        val stateAfterBuilt = progressedState.copy(
            ores = progressedState.ores - print.clayOreNeeded, clayProduction = progressedState.clayProduction + 1
        )

        val result = searchOres(stateAfterBuilt, print)
        max = max(result, max)
    }

    // Wait & build ore robot
    if (state.oreProduction > 0) {
        val oresNeeded = max(0, print.oreOreNeeded - state.ores)
        val roundsToWait = if (oresNeeded == 0) {
            1
        } else {
            ceil(oresNeeded.toFloat() / state.oreProduction.toFloat()).toInt() + 1
        }
        val progressedState = progressState(state, roundsToWait)
        val stateAfterBuilt = progressedState.copy(
            ores = progressedState.ores - print.oreOreNeeded, oreProduction = progressedState.oreProduction + 1
        )

        val result = searchOres(stateAfterBuilt, print)
        max = max(result, max)
    }

    return max
}


fun day19_1(lines: List<String>): Any {
    val bluePrints = lines.map { it.allInts() }
        .map { ints -> BluePrint(ints[0], ints[1], ints[2], ints[3], ints[4], ints[5], ints[6]) }
    val startState = MineralState(0, 0, 0, 0, 0, 1, 0, 0, 0)

    return bluePrints.map { print ->
        highestSoFar = 0
        prunedCount = 0
        val result = searchOres(startState, print)
        println("${print.id}: Result=$result, prunedCount=$prunedCount")
        print.id * result
    }.sum()

}


fun day19_2(lines: List<String>): Any {
    maxMinutes = 32
    val bluePrints = lines.take(3).map { it.allInts() }
        .map { ints -> BluePrint(ints[0], ints[1], ints[2], ints[3], ints[4], ints[5], ints[6]) }
    val startState = MineralState(0, 0, 0, 0, 0, 1, 0, 0, 0)

    return bluePrints.map { print ->
        highestSoFar = 0
        prunedCount = 0
        val result = searchOres(startState, print)
        println("${print.id}: Result=$result, prunedCount=$prunedCount")
        result
    }.fold(1) { acc, res -> res * acc }

}

fun main() {

//    run("1", fileName = "day19_ex.txt", func = ::day19_1)
//    run("2", fileName = "day19_ex.txt", func = ::day19_2)

    run("1", fileName = "day19.txt", func = ::day19_1)
    run("2", fileName = "day19.txt", func = ::day19_2)
}

/*
OUTPUT
======
1: Result=3, prunedCount=96495
2: Result=0, prunedCount=0
...
29: Result=15, prunedCount=50267
30: Result=1, prunedCount=1692830
Done. Took 1926ms to run
Result for 1 (day19.txt):	XXXX
Copied to clipboard!

1: Result=38, prunedCount=973204
2: Result=16, prunedCount=9364291
3: Result=17, prunedCount=151150011
Done. Took 3034ms to run
Result for 2 (day19.txt):	XXXX
 */