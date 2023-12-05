package ren.iamka.aoc23.day5

import ren.iamka.aoc23.readLines
import kotlin.math.max
import kotlin.math.min

fun main() {
    parseAlmanac {
        this.seedsToLocationPart1()
    }
    parseAlmanac {
        this.seedsToLocationPart2()
    }
}

data class Almanac(val seeds: List<Long>, val maps: List<CategoryMap>)

data class CategoryMap(val categoryMapEntries: List<CategoryMapEntry>)
data class CategoryMapEntry(val destinationRangeStart: Long, val sourceRangeStart: Long, val rangeLength: Long)

private fun parseAlmanac(operation: Almanac.() -> Unit) {
    return "/day5/data.txt".readLines {
        val seeds = mutableListOf<Long>()
        val maps = mutableListOf<CategoryMap>()
        val currentMapEntries = mutableListOf<CategoryMapEntry>()
        forEach { lineUntrimmed ->
            val line = lineUntrimmed.trim()
            if (line.startsWith("seeds: ")) {
                seeds.addAll(line.split(" ")
                    .map { it.trim() }
                    .filter { it.all { char -> char.isDigit() } }
                    .map { it.toLong() })
            } else if (line.endsWith("map:")) {
                // previous map is over
                if (currentMapEntries.isNotEmpty()) {
                    maps.add(CategoryMap(categoryMapEntries = currentMapEntries.toList()))
                    currentMapEntries.clear()
                }
            } else if (line.isNotEmpty()) {
                val (dest, src, len) = line.split(" ").map { it.trim().toLong() }
                currentMapEntries.add(
                    CategoryMapEntry(destinationRangeStart = dest, sourceRangeStart = src, rangeLength = len)
                )
            }
        }
        // Add last map
        if (currentMapEntries.isNotEmpty()) {
            maps.add(CategoryMap(categoryMapEntries = currentMapEntries.toList()))
            currentMapEntries.clear()
        }
        Almanac(seeds = seeds.toList(), maps = maps.toList()).operation()
    }
}

private fun Almanac.seedsToLocationPart1() {
    seeds.minOfOrNull { seed ->
        this.maps.fold(initial = seed) { acc, map ->
            acc.mapSourceToDest(map)
        }
    }.apply { println("part 1: $this") }
}

private fun Almanac.seedsToLocationPart2() {
    val seedRanges = seeds.chunked(2).map { (start, length) ->
        (start until start + length)
    }
    seedRanges.minOfOrNull { range ->
        val result = maps.fold(listOf(range)) { acc, map ->
            acc.map { it.mapSourceToDest(map) }.flatten()
        }
        result.minBy { it.first }.first
    }.apply { println("part 2: $this") }
}

private fun Long.mapSourceToDest(map: CategoryMap): Long {
    map.categoryMapEntries.forEach {
        if (this in it.sourceRangeStart until it.sourceRangeStart + it.rangeLength) {
            return it.destinationRangeStart + (this - it.sourceRangeStart)
        }
    }
    // Nothing found, return as-is
    return this
}

private fun LongRange.mapSourceToDest(map: CategoryMap): List<LongRange> {
    val destRanges = mutableListOf<LongRange>()
    val sorted = map.categoryMapEntries.sortedBy { it.sourceRangeStart }

    var currentValue = this.first

    sorted.forEach { entry ->
        val srcRange = entry.sourceRangeStart until entry.sourceRangeStart + entry.rangeLength
        val srcToDest = -(entry.sourceRangeStart - entry.destinationRangeStart)
        if (this.overlaps(srcRange)) {
            val newRangeStart = max(this.first, srcRange.first)
            destRanges.add(currentValue until newRangeStart)
            val newRangeEnd = min(this.last, srcRange.last)
            destRanges.add(newRangeStart + srcToDest..newRangeEnd + srcToDest)
            currentValue = newRangeEnd + 1
        }
    }

    destRanges.add(currentValue..this.last)
    return destRanges.filter { !it.isEmpty() }.ifEmpty { listOf(this) }
}

private fun LongRange.overlaps(other: LongRange): Boolean {
    return !(this.last < other.first || this.first > other.last)
}