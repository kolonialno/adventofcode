package com.matsemann.adventofcode2022

import com.matsemann.adventofcode2022.utils.*

data class Valve(val name: String, var tunnelsTo: List<Valve> = mutableListOf(), var flowRate: Int = 0) {
    override fun toString(): String {
        return "Valve(name='$name')"
    }
}

fun day16_1(lines: List<String>): Any {
    val valves = DefaultMap<String, _> { Valve(it) }
    lines.map {
        val parts = it.split("; ")
        val tunnels = parts[1].substringAfter(" to ").split(" ").drop(1)

        val valve = valves[parts[0].drop(6).take(2)]
        valve.flowRate = parts[0].allInts()[0]
        valve.tunnelsTo = tunnels.map { tunnel -> valves[tunnel.take(2)] }
    }


    fun score(valve: Valve, seenValves: Set<String>, openValves: Set<String>, roundsLeft: Int): Int {
        if (roundsLeft <= 0) {
            return 0
        }
        val scores = mutableListOf<Int>()
        // If we stay and open the valve
        if (valve.name !in openValves && valve.flowRate > 0) {
            val nowOpenValves = openValves + valve.name
            val nowSeenValves = setOf(valve.name)
            val lifeTimeValue = (roundsLeft - 1) * valve.flowRate
            for (v in valve.tunnelsTo) {
                val theirScore = score(v, nowSeenValves, nowOpenValves, roundsLeft - 2)
                scores.add(lifeTimeValue + theirScore)
            }
        }
        // If we just move on
        val nowSeenValves = seenValves + valve.name
        for (v in valve.tunnelsTo) {
            if (v.name !in seenValves) { // avoid stupid loops
                val theirScore = score(v, nowSeenValves, openValves, roundsLeft - 1)
                scores.add(theirScore)
            }
        }
        if (scores.isEmpty()) return 0
        return scores.max()
    }

    return score(valves["AA"], setOf(), setOf(), 30)

}

fun main() {

    run("1", fileName = "day16_ex.txt", func = ::day16_1)
//    run("2", fileName = "day16_ex.txt", func = ::day16_2)

    run("1", fileName = "day16.txt", func = ::day16_1)
//    run("2", fileName = "day16.txt", func = ::day16_2)
}

/*
OUTPUT
======


 */