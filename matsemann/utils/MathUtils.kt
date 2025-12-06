package com.matsemann.adventofcode2025.utils

import java.math.BigInteger
import kotlin.math.abs
import kotlin.math.sqrt

/**
 * Takes a list of ints and concats them
 * [5, 4, 13] = 5413
 */
fun List<Int>.concat() = this.joinToString(separator = "") { it.toString() }.toInt()

/**
 * Multiplies all elements in the list together
 */
fun List<Int>.product() = this.reduce { acc, current -> acc * current }
fun List<BigInteger>.product() = this.reduce { acc, current -> acc * current }

/*
Some utils for making big integer map more smooth
 */
inline fun Int.big() = this.toBigInteger()
inline operator fun BigInteger.times(other: Int): BigInteger = this * other.toBigInteger()
inline operator fun BigInteger.plus(other: Int) = this + other.toBigInteger()
inline operator fun BigInteger.minus(other: Int): BigInteger = this - other.toBigInteger()

/**
 * Mod starts at 1 and goes to max,
 * (instead of 0 and to max-1)
 */
infix fun Int.mod1To(max: Int): Int {
    return (this - 1) % max + 1
}

/**
 * A vector representing 2D numbers.
 * Immutable.
 */
data class Vec2d(val x: Double, val y: Double) {
    operator fun plus(other: Vec2d) = Vec2d(x + other.x, y + other.y)
    operator fun plus(other: IntVec) = Vec2d(x + other.x, y + other.y)
    operator fun plus(dir: Direction) = Vec2d(x + dir.x, y + dir.y)

    operator fun minus(other: Vec2d) = Vec2d(x - other.x, y - other.y)
    operator fun unaryMinus() = Vec2d(-x, -y)

    operator fun times(factor: Double) = Vec2d(x * factor, y * factor)

    fun manhattan(other: Vec2d) = abs(x - other.x) + abs(y - other.y)
    fun dst(other: Vec2d) = sqrt((x - other.x) * (x - other.x) + (y - other.y) * (y - other.y))
    fun len() = dst(zero)
    fun norm() = times(1 / len())
    fun dot(other: Vec2d) = x * other.x + y * other.y
    fun cross(other: Vec2d) = x * y - y * x

    fun isPerpendicular(other: Vec2d) = abs(dot(other)) < 0.000001
    fun hasSameDirection(other: Vec2d) = dot(other) > 0.0
    fun hasOppositeDirection(other: Vec2d) = dot(other) < 0.0

    companion object {
        val zero = Vec2d(0.0, 0.0)
    }
}

data class Rectangle(val pos1: IntVec, val pos2: IntVec) {

    operator fun contains(vec: IntVec): Boolean {
        return vec.x in pos1.x..pos2.x && vec.y in pos1.y..pos2.y
    }

    // Whether the rectangle is to the side of the point
    fun isBelow(y: Int) = y > pos2.y
    fun isAbove(y: Int) = y < pos1.y
    fun isRightOf(x: Int) = x < pos1.x
    fun isLeftOf(x: Int) = pos2.x < x

    // The point to the rectangle
    fun pointIsBelow(y: Int) = y < pos1.y
    fun pointIsAbove(y: Int) = y > pos2.y
    fun pointIsRightOf(x: Int) = pos2.x < x
    fun pointIsLeftOf(x: Int) = x < pos1.x

    companion object {
        fun fromNumbers(x1: Int, y1: Int, x2: Int, y2: Int): Rectangle {
            // Sorts them so bottom left corner is pos1
            val xs = listOf(x1, x2).sorted()
            val ys = listOf(y1, y2).sorted()
            return Rectangle(
                IntVec(xs[0], ys[0]),
                IntVec(xs[1], ys[1])
            )
        }

        fun fromVecs(vec1: IntVec, vec2: IntVec): Rectangle {
            return fromNumbers(vec1.x, vec1.y, vec2.x, vec2.y)
        }
    }

}

fun symbolToOp(symbol: String): (Int, Int) -> Int {
    when (symbol) {
        "*" -> return Int::times
        "+" -> return Int::plus
        "-" -> return Int::minus
        "/" -> return Int::div
        else -> throw Exception("wtf is $symbol")
    }
}

fun symbolToLongOp(symbol: String): (Long, Long) -> Long {
    when (symbol) {
        "*" -> return Long::times
        "+" -> return Long::plus
        "-" -> return Long::minus
        "/" -> return Long::div
        else -> throw Exception("wtf is $symbol")
    }
}
