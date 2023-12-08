package ren.iamka.aoc23.day8

import ren.iamka.aoc23.readLines
import java.lang.IllegalArgumentException

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
        val directionsIterator = generateSequence(0) { it + 1 }.map { instructions[it % instructions.length] }.iterator()
        while (currentValue != "ZZZ"){
            val direction = directionsIterator.next()
            val (left, right) = map[currentValue]!!
            currentValue = when (direction){
                'L' -> left
                'R' -> right
                else -> throw IllegalArgumentException("Invalid direction $direction")
            }
            steps++
        }
    }
    return steps
}