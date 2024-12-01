package com.matsemann.adventofcode2024.utils

import kotlin.math.abs
import kotlin.math.max
import kotlin.math.min
import kotlin.math.sign

enum class Direction(var x: Int, var y: Int) {
    RIGHT(1, 0),
    DOWN(0, -1),
    LEFT(-1, 0),
    UP(0, 1);

    fun turnCw() =
        when (this) {
            RIGHT -> DOWN
            DOWN -> LEFT
            LEFT -> UP
            UP -> RIGHT
        }

    fun turnCcw() =
        when (this) {
            RIGHT -> UP
            DOWN -> RIGHT
            LEFT -> DOWN
            UP -> LEFT
        }

    fun flip() =
        when (this) {
            RIGHT -> LEFT
            LEFT -> RIGHT
            UP -> DOWN
            DOWN -> UP
        }

    fun toVec() = IntVec(x, y)

    companion object {
        fun fromNSEW(ch: Char) =
            when (ch) {
                'N' -> UP
                'S' -> DOWN
                'E' -> RIGHT
                'W' -> LEFT
                else -> throw Exception("unkown direction $ch")
            }


        fun fromUDLR(str: String) =
            when (str.uppercase()[0]) {
                'U' -> UP
                'D' -> DOWN
                'R', 'F' -> RIGHT
                'L' -> LEFT
                else -> throw Exception("unkown direction $str")

            }


        fun setYDown() {
            DOWN.y = 1
            UP.y = -1
        }
    }
}

data class IntVec(val x: Int, val y: Int) {
    operator fun plus(other: IntVec) = IntVec(x + other.x, y + other.y)
    operator fun plus(dir: Direction) = IntVec(x + dir.x, y + dir.y)

    operator fun minus(other: IntVec) = IntVec(x - other.x, y - other.y)
    operator fun unaryMinus() = IntVec(-x, -y)

    operator fun times(factor: Int) = IntVec(x * factor, y * factor)

    fun manhattan(other: IntVec) = abs(x - other.x) + abs(y - other.y)
    fun manhattan() = manhattan(zero)
    fun chebyshev(other: IntVec) = max(abs(x - other.x), abs(y - other.y))
    fun chebyshev() = chebyshev(zero)

    fun asDir() = IntVec(x.sign, y.sign)

    fun mirrorX(axis: Int = 0) = IntVec(axis - (x - axis), y)
    fun mirrorY(axis: Int = 0) = IntVec(x, axis - (y - axis))

    // Note, it is inclusive, so for indexing reduce with 1
    fun withinBounds(bounds: IntVec) = withinBounds(0, bounds.x, 0, bounds.y)
    fun withinBounds(minX: Int, maxX: Int, minY: Int, maxY: Int) =
        x in minX..maxX && y in minY..maxY

    fun neighbors() = Direction.values().map { this + it }
    fun neighbors(bounds: IntVec) = Direction.values().map { this + it }
        .filter { it.withinBounds(bounds) }

    fun neighbors9() = (-1..1)
        .flatMap { x ->
            (-1..1)
                .map { y -> IntVec(x, y) }
        }
        .filter { it.x != 0 || it.y != 0 }
        .map { this + it }

    fun neighbors9(bounds: IntVec) =
        neighbors9().filter { it.withinBounds(bounds) }

    companion object {
        val zero = IntVec(0, 0)

        /**
         * (5,2),(7,4)=>(5,2),(6,3),(7,4)
         */
        fun IntVec.allBetween(other: IntVec): List<IntVec> {
            val diff = this.chebyshev(other)
            val dir = (other - this).asDir()
            return (0..diff).map { this + (dir * it) }
        }

        /**
         * Returns all positions inside the grid bounds,
         * iteration order is line by line
         */
        fun IntVec.allWithinBounds() = (0..y).flatMap { y1 ->
            (0..x).map { x1 -> IntVec(x1, y1) }
        }

        fun fromStr(str: String, delim: String = ","): IntVec {
            val split = str.split(delim)
            return IntVec(split[0].toInt(), split[1].toInt())
        }

        fun String.toIntVec(delim: String = ",") = fromStr(this, delim)

        /**
         * Calculates the min and max of x and y for all the vectors
         */
        fun Iterable<IntVec>.bounds() = this.fold(
            listOf(
                Int.MAX_VALUE,
                Int.MIN_VALUE,
                Int.MAX_VALUE,
                Int.MIN_VALUE
            )
        ) { acc, vec ->
            listOf(
                min(acc[0], vec.x),
                max(acc[1], vec.x),
                min(acc[2], vec.y),
                max(acc[3], vec.y)
            )
        }

        fun Iterable<IntVec>.showAsGrid(char: Char = fullBlock): String {
            val (minX, maxX, minY, maxY) = this.bounds()
            return this.showAsGrid(minX..maxX, minY..maxY, char)
        }

        fun Iterable<IntVec>.showAsGrid(xRange: IntRange, yRange: IntRange, char: Char = fullBlock): String {
            return yRange.joinToString("\n") { y ->
                xRange.joinToString("") { x ->
                    if (IntVec(x, y) in this) {
                        char.toString()
                    } else {
                        " "
                    }
                }
            }
        }

        fun IntVec.showAsGrid(posFunc: (IntVec) -> Char): String {
            return (0..y).joinToString("\n") { y2 ->
                (0..x).joinToString("") { x2 ->
                    posFunc(IntVec(x2, y2)).toString()
                }
            }
        }

    }
}

fun <E> List<List<E>>.bounds() = IntVec(this[0].size - 1, this.size - 1)

operator fun <E> List<List<E>>.get(intVec: IntVec) = this[intVec.y][intVec.x]
operator fun <E> List<MutableList<E>>.set(intVec: IntVec, value: E) {
    this[intVec.y][intVec.x] = value
}


val fullBlock = '█'
