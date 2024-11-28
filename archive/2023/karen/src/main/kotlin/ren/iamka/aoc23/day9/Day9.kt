package ren.iamka.aoc23.day9

import ren.iamka.aoc23.readLines
import ren.iamka.aoc23.toInts

fun main() {
    part1()
    part2()
}

fun part1() {
    parse {
        reversed().fold(0L) { i, changes ->
            changes.last() + i
        }
    }
}

fun part2() {
    parse {
        reversed().fold(0L) { i, changes ->
            changes.first() - i
        }
    }
}

fun parse(operation: List<List<Int>>.() -> Long) {
    "/day9/data.txt".readLines {
        val lines = this.toList()
        lines.sumOf { line ->
            val sequence = line.toInts()
            val changesList: MutableList<List<Int>> = mutableListOf()
            changesList.add(sequence)
            while (changesList[changesList.lastIndex].any { it != 0 }) {
                changesList.add(changesList.last().getChanges())
            }
            changesList.operation()
        }.apply { println(this) }
    }
}


fun List<Int>.getChanges(): List<Int> {
    return (0 until this.lastIndex).map { i ->
        this[i + 1] - this[i]
    }
}