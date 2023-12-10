package ren.iamka.aoc23.day10

import ren.iamka.aoc23.readLines
import kotlin.math.ceil

fun main() {
    parse()
}

fun parse() {
    val map = mutableListOf<String>()
    var indexAnimal = -1 to -1
    "/day10/data.txt".readLines {
        this.forEachIndexed { index, s ->
            map.add(s)
            s.indexOf('S').apply {
                if (this != -1) {
                    indexAnimal = index to this
                }
            }
        }
    }

    // check where to start the loop.
    var steps = 1
    val alreadyVisited = mutableSetOf<Pair<Int, Int>>()
    var coords = -1 to -1
    val directions = listOf(Direction.North, Direction.East, Direction.South, Direction.West)
    directions.forEach { direction ->
        val transformed = direction.transform(indexAnimal.first, indexAnimal.second)
        if (map[transformed.first][transformed.second] in direction.validPipes) {
            coords = transformed
        }
    }

   while (map[coords.first][coords.second] != 'S') {
        coords = coords.goToNext(map, alreadyVisited)
        steps++
    }

    println(ceil(steps.toFloat() / 2).toInt())

}

sealed class Direction {
    abstract val validPipes: List<Char>
    abstract val transform: (y: Int, x: Int) -> Pair<Int, Int>

    data object North : Direction() {
        override val validPipes = listOf('|', '7', 'F')
        override val transform = { y: Int, x: Int ->
            (y - 1 to x)
        }
    }

    data object South : Direction() {
        override val validPipes = listOf('|', 'L', 'J')
        override val transform = { y: Int, x: Int ->
            (y + 1 to x)
        }
    }

    data object West : Direction() {
        override val validPipes = listOf('-', 'F', 'L')
        override val transform = { y: Int, x: Int ->
            (y to x - 1)
        }
    }

    data object East : Direction() {
        override val validPipes = listOf('-', '7', 'J')
        override val transform = { y: Int, x: Int ->
            (y to x + 1)
        }
    }
}

val pipeToDirections = mapOf(
    '|' to listOf(Direction.North, Direction.South),
    '-' to listOf(Direction.West, Direction.East),
    'L' to listOf(Direction.North, Direction.East),
    'J' to listOf(Direction.North, Direction.West),
    '7' to listOf(Direction.South, Direction.West),
    'F' to listOf(Direction.South, Direction.East),
    )

fun Pair<Int, Int>.goToNext(map: List<String>, alreadyVisited: MutableSet<Pair<Int, Int>>): Pair<Int, Int> {
    alreadyVisited.add(this)
    val value = map[first][second]

    pipeToDirections[value]?.let { directions ->
        directions.forEach { direction ->
            val target = direction.transform(this.first, this.second)
            if (target !in alreadyVisited){
                return target
            }
        }
    }
    throw IllegalArgumentException("Can't find connection for $value at $first $second")
}