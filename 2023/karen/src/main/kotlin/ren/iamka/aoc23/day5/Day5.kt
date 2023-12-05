package ren.iamka.aoc23.day5

import ren.iamka.aoc23.readLines

fun main() {
    parseAlmanac {
       this.seedsToLocation()
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

private fun Almanac.seedsToLocation() {
    this.seeds.minOfOrNull { seed ->
        this.maps.fold(initial = seed) { acc, map ->
            acc.mapSourceToDest(map)
        }
    }.apply { println(this) }
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