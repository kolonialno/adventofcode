package com.matsemann.adventofcode2025.utils

import com.matsemann.adventofcode2025.utils.Direction.DOWN
import com.matsemann.adventofcode2025.utils.Direction.DOWNLEFT
import com.matsemann.adventofcode2025.utils.Direction.DOWNRIGHT
import com.matsemann.adventofcode2025.utils.Direction.LEFT
import com.matsemann.adventofcode2025.utils.Direction.RIGHT
import com.matsemann.adventofcode2025.utils.Direction.UP
import com.matsemann.adventofcode2025.utils.Direction.UPLEFT
import com.matsemann.adventofcode2025.utils.Direction.UPRIGHT
import kotlin.math.abs
import kotlin.math.max
import kotlin.math.min
import kotlin.math.sign

object Direction {
    var RIGHT = IntVec(1, 0)
    var DOWN = IntVec(0, -1)
    var LEFT = IntVec(-1, 0)
    var UP = IntVec(0, 1)

    var UPRIGHT = RIGHT + UP
    var UPLEFT = LEFT + UP
    var DOWNRIGHT = RIGHT + DOWN
    var DOWNLEFT = LEFT + DOWN

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

    fun fromArrows(char: Char) =
        when (char) {
            '^' -> UP
            'v' -> DOWN
            '>' -> RIGHT
            '<' -> LEFT
            else -> throw Exception("unkown direction $char")
        }

    /**
     * When used in a grid context, where Y grows "down" when you print
     * the list, makes it easier to reason about
     */
    fun setYDown() {
        DOWN = IntVec(0, 1)
        UP = IntVec(0, -1)

        UPRIGHT = RIGHT + UP
        UPLEFT = LEFT + UP
        DOWNRIGHT = RIGHT + DOWN
        DOWNLEFT = LEFT + DOWN
    }
}

data class IntVec(val x: Int, val y: Int) {
    operator fun plus(other: IntVec) = IntVec(x + other.x, y + other.y)

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

    fun flip() = unaryMinus()

    // Both handle both Y up/down
    fun rotateClockwise() = IntVec(y, -x) * Direction.UP.y
    fun rotateCounterClockwise() = IntVec(y, -x) * Direction.UP.y

    // Note, it is inclusive, so for indexing reduce with 1
    fun withinBounds(bounds: IntVec) = withinBounds(0, bounds.x, 0, bounds.y)
    fun withinBounds(minX: Int, maxX: Int, minY: Int, maxY: Int) =
        x in minX..maxX && y in minY..maxY

    fun neighbors() = listOf(UP, DOWN, LEFT, RIGHT).map { this + it }
    fun neighbors(bounds: IntVec) = neighbors().filter { it.withinBounds(bounds) }

    fun neighbors9() = listOf(UPLEFT, UP, UPRIGHT, LEFT, RIGHT, DOWNLEFT, DOWN, DOWNRIGHT).map { this + it }
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

/**
 * @return grid to bounds
 */
fun List<String>.toCharGrid(): Pair<List<List<Char>>, IntVec> {
    val grid = this.map { it.toList() }
    val bounds = grid.bounds()
    return grid to bounds
}

fun <E> List<List<E>>.bounds() = IntVec(this[0].size - 1, this.size - 1)

operator fun <E> List<List<E>>.get(intVec: IntVec) = this[intVec.y][intVec.x]
operator fun <E> List<MutableList<E>>.set(intVec: IntVec, value: E) {
    this[intVec.y][intVec.x] = value
}


val fullBlock = '█'
