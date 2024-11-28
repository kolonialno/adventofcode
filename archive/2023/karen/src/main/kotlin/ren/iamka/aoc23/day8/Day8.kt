package ren.iamka.aoc23.day8

import ren.iamka.aoc23.readLines
import java.util.*

fun main() {
    println(part1())
    println(part2())
}

fun parse(operation: (map: SortedMap<String, Pair<String, String>>, directions: Iterator<Char>) -> Long): Long {
    var lines: List<String> = emptyList()
    "/day8/data.txt".readLines {
        lines = this.toList()
    }
    val instructions = lines.first()
    val regex = Regex("[A-Z]{3}")
    val map = lines.takeLast(lines.size - 2).associate {
        val (key, left, right) = regex.findAll(it).map { it.value }.toList()
        key to (left to right)
    }.toSortedMap()

    val directionsIterator =
        generateSequence(0L) { it + 1 }.map { instructions[(it % instructions.length.toLong()).toInt()] }.iterator()

    return operation(map, directionsIterator)
}

fun part1(): Long {
    return parse { map, directions ->
        var steps = 0L
        var currentKey = map.firstKey()

        while (currentKey != "ZZZ") {
            val direction = directions.next()
            currentKey = nextKey(key = currentKey, map = map, direction = direction)
            steps++
        }
        steps
    }
}

fun part2(): Long {
    return parse { map, directions ->
        val keys = map.keys.filter { it.endsWith("A") }
        findLCMOfListOfNumbers(keys.map { key ->
            var newKey = key
            var steps = 0L
            while (!newKey.endsWith("Z")) {
                val direction = directions.next()
                newKey = nextKey(key = newKey, map = map, direction = direction)
                steps++
            }
            steps
        })
    }
}

fun nextKey(key: String, map: SortedMap<String, Pair<String, String>>, direction: Char): String {
    val (left, right) = map[key]!!
    return when (direction) {
        'L' -> left
        'R' -> right
        else -> throw IllegalArgumentException("Invalid direction $direction")
    }
}

// source: internet 🌚
fun findLCM(a: Long, b: Long): Long {
    val larger = if (a > b) a else b
    val maxLcm = a * b
    var lcm = larger
    while (lcm <= maxLcm) {
        if (lcm % a == 0L && lcm % b == 0L) {
            return lcm
        }
        lcm += larger
    }
    return maxLcm
}

fun findLCMOfListOfNumbers(numbers: List<Long>): Long {
    var result = numbers[0]
    for (i in 1 until numbers.size) {
        result = findLCM(result, numbers[i])
    }
    return result
}