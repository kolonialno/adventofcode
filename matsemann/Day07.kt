package com.matsemann.adventofcode2021

import kotlin.math.abs

// My initial solutions
// dirty and not very fast (part2 takes about a second),
// but was very quick to write when trying to finish fast

fun day07_1(lines: List<String>) =
    lines.first().split(",").map { it.toInt() }.let { crabs ->
        (0..crabs.maxOf { it }).minOf { pos ->
            crabs.sumOf { abs(it - pos) }
        }
    }


fun day07_2(lines: List<String>) =
    lines.first().split(",").map { it.toInt() }.let { crabs ->
        (0..crabs.maxOf { it }).minOf { pos ->
            crabs.sumOf {
                (0..abs(it - pos)).sum() // slower than calculating arithmetic sum, but quicker to write 🤷‍
            }
        }
    }


// Improvements
// Had this idea originally, but didn't know if it was gonna work mathematically
// so went with the other approach first
fun day07_1_2(lines: List<String>) : Any {
    val crabs = lines.first().split(",").map { it.toInt() }
    val median = crabs.sorted()[crabs.size/2]
    return crabs.sumOf { abs(it - median) }
}
fun day07_2_2(lines: List<String>) : Any {
    val crabs = lines.first().split(",").map { it.toInt() }
    val avg = crabs.sum() / crabs.size
    return crabs.sumOf { abs(it - avg).let {dst -> (dst * (dst + 1)) / 2} }
}


fun main() {
    run("1", fileName = "day07_1.txt", func = ::day07_1)
    run("1_2", fileName = "day07_1.txt", func = ::day07_1_2)

    run("2", fileName = "day07_1.txt", func = ::day07_2)
    run("2_2", fileName = "day07_1.txt", func = ::day07_2_2)
}

/*
OUTPUT
======

Done. Took 5ms to run
Result for 1:	343468
Copied to clipboard!

Done. Took 1ms to run
Result for 1:	343468
Copied to clipboard!

Done. Took 1110ms to run
Result for 2:	96086265
Copied to clipboard!

Done. Took 0ms to run
Result for 2:	96086265
Copied to clipboard!

 */
