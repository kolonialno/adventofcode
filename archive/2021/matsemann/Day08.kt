package com.matsemann.adventofcode2021

fun day08_1(lines: List<String>): Any {
    return lines.map { it.split(" | ") }
        .flatMap { it[1].split(" ") }
        .count { it.length == 2 || it.length == 4 || it.length == 3 || it.length == 7 }
}

/**
 * Handcrafted solution after doing it manually on paper first 🤔
 */
fun day08_2(lines: List<String>): Any {
    return lines.map { line ->
        val parts = line.split(" | ")

        val obs = parts[0].split(" ").map { it.toSet() }

        // Know these ones based on unique length
        val mappings = mutableMapOf(
            1 to obs.first { it.size == 2 },
            4 to obs.first { it.size == 4 },
            7 to obs.first { it.size == 3 },
            8 to obs.first { it.size == 7 },
        )

        // 6 is the one of length 6 (0, 6 or 9) with 1 overlap between "1"
        mappings[6] = obs.filter { it.size == 6 }
            .first { (it intersect mappings[1]!!).size == 1 }

        // 3 is the one of length 5 (2, 3 or 5) with 3 overlaps with "7"
        mappings[3] = obs.filter { it.size == 5 }
            .first { (it intersect mappings[7]!!).size == 3 }

        // panel top right is the one in "1" not present in "6"
        val cccc = (mappings[1]!! - mappings[6]!!).first()
        // And 2 is then the one of (2 or 5) remaining having that panel
        mappings[2] = obs.filter { it.size == 5 }
            .filter { !mappings.values.contains(it) }
            .first { it.contains(cccc) }
        // 5 is the last remaining of length 5
        mappings[5] = obs.filter { it.size == 5 }
            .first { !mappings.values.contains(it) }

        // Of 0 and 9 remaining, 0 is the one with 4 overlaps with 5
        mappings[0] = obs.filter { it.size == 6 }
            .filter { !mappings.values.contains(it) }
            .first { (it intersect mappings[5]!!).size == 4 }

        mappings[9] = obs.filter { it.size == 6 }
            .first { !mappings.values.contains(it) }


        parts[1].split(" ").joinToString("") { d ->
            mappings.entries.first { it.value == d.toSet() }.key.toString()
        }.toInt()

    }.sum()

}

fun main() {
//    run("1", fileName = "day08_ex.txt", func = ::day08_1)
    run("1", fileName = "day08_1.txt", func = ::day08_1)
//    run("2", fileName = "day08_ex.txt", func = ::day08_2)
    run("2", fileName = "day08_1.txt", func = ::day08_2)
//    run("2", fileName = "day08_1.txt", func = ::day08_2_2)
}

/*
OUTPUT
======

Done. Took 0ms to run
Result for 1:	303
Copied to clipboard!

Done. Took 6ms to run
Result for 2:	961734
Copied to clipboard!

 */