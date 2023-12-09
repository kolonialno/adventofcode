package ren.iamka.aoc23.day8

import ren.iamka.aoc23.readLines

fun main() {
    println(part1())
}

fun part1(): Int {
    var steps = 0
    "/day8/data.txt".readLines {
        val lines = this.toList()
        val instructions = lines.first()
        val regex = Regex("[A-Z]{3}")
        val map = lines.takeLast(lines.size - 2).associate {
            val (key, left, right) = regex.findAll(it).map { it.value }.toList()
            key to (left to right)
        }.toSortedMap()

        var currentValue = map.firstKey()
        val directionsIterator =
            generateSequence(0) { it + 1 }.map { instructions[it % instructions.length] }.iterator()
        while (currentValue != "ZZZ") {
            val direction = directionsIterator.next()
            val (left, right) = map[currentValue]!!
            currentValue = when (direction) {
                'L' -> left
                'R' -> right
                else -> throw IllegalArgumentException("Invalid direction $direction")
            }
            steps++
        }
    }
    return steps
}

/*fun part2(): Unit {
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

    var keys = map.keys.filter { it.endsWith("A") }
    var steps = 0
    val directionsIterator = generateSequence(0L) { it + 1 }.map { instructions[(it % instructions.length.toLong()).toInt()] }.iterator()

    while(!keys.all { it.endsWith("Z") }){
        val direction = directionsIterator.next()
        keys = keys.map { nextKey(key = it, map, direction) }
        steps++
    }

    println(steps)
}

fun getSteps(key: String, map: SortedMap<String, Pair<String, String>>, instructions: String): Int {
    println("Launch $key")
    var steps = 0
    val directionsIterator = generateSequence(0) { it + 1 }.map { instructions[it % instructions.length] }.iterator()
    var currentKey = key

    while (!currentKey.endsWith("Z")) {
        val direction = directionsIterator.next()
        val (left, right) = map[currentKey]!!
        currentKey = when (direction) {
            'L' -> left
            'R' -> right
            else -> throw IllegalArgumentException("Invalid direction $direction")
        }
        // println("go to $currentKey")
        steps++
    }
    println("Key $key steps $steps")
    return steps
}

fun nextKey(key: String, map: SortedMap<String, Pair<String, String>>, direction: Char): String {
    val (left, right) = map[key]!!
    return when (direction) {
        'L' -> left
        'R' -> right
        else -> throw IllegalArgumentException("Invalid direction $direction")
    }
}
*/