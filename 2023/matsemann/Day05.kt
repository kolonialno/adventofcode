package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*
import java.math.BigInteger

fun day05_1(lines: List<String>): Any {
    val seeds = lines.first().split(" ").bigInts()
    val categories = lines
        .drop(2)
        .splitBy { it == "" }
        .map { maps ->
            maps.drop(1).map {
                it.split(" ").bigInts()
            }
        }

    return seeds.map { seed ->
        categories.fold(seed) { current, mappings ->
            val mapping = mappings.find { mapping ->
                current in mapping[1]..<mapping[1] + mapping[2]
            }

            if (mapping == null) {
                current
            } else {
                val diff = current - mapping[1]
                val newValue = mapping[0] + diff
                newValue
            }
        }
    }.min()
}


fun day05_2(lines: List<String>): Any {
    val seedRanges = lines.first().split(" ").bigInts()
        .chunked(2).map { it[0] to it[1] }
    val categories = lines
        .drop(2)
        .splitBy { it == "" }
        .map { maps ->
            maps.drop(1).map {
                it.split(" ").bigInts()
            }
        }

    return seedRanges.map { seedRange ->
        categories.fold(listOf(seedRange)) { ranges, mappings ->

            val rangesToMap = ranges.toMutableList()
            val mappedRanges = mutableListOf<Pair<BigInteger, BigInteger>>()

            while (rangesToMap.isNotEmpty()) {
                val range = rangesToMap.removeFirst()

                val start = range.first
                val end = start + range.second

                val mapping = mappings.find { (_, mapStart, mapRange) ->
                    val mapEnd = mapStart + mapRange
                    start < mapEnd && end > mapStart
                }

                if (mapping == null) {
                    mappedRanges += range
                    continue
                }

                val (dest, mapStart, mapRange) = mapping
                val mapEnd = mapStart + mapRange

                val mappedStart = start.max(mapStart)
                val mappedEnd = end.min(mapEnd)
                val mappedDiff = mappedEnd - mappedStart

                val startDiff = mappedStart - mapStart
                mappedRanges += (dest + startDiff) to mappedDiff

                if (start < mapStart) {
                    rangesToMap += start to (mapStart - start)
                }
                if (end > mapEnd) {
                    rangesToMap += mapEnd to (end - mapEnd)
                }
            }

            mappedRanges
        }
    }.flatten().println().minOf { it.first }


}

fun main() {

//    run("1", fileName = "day05_ex.txt", func = ::day05_1)
    run("2", fileName = "day05_ex.txt", func = ::day05_2)


//    run("1", fileName = "day05.txt", func = ::day05_1)
    run("2", fileName = "day05.txt", func = ::day05_2)
}
