package com.matsemann.adventofcode2021

import java.awt.Toolkit
import java.awt.datatransfer.StringSelection
import java.nio.file.Files
import java.nio.file.Path
import kotlin.math.abs
import kotlin.math.max
import kotlin.math.sign
import kotlin.math.sqrt


fun getFileLines(fileName: String) =
    Files.readAllLines(Path.of("adventofcode2021/inputs/$fileName"))

fun run(runName: String? = null, fileName: String, func: (List<String>) -> Any) {
    val lines = getFileLines(fileName)
    val result = measure { func(lines) }

    println("Result for ${runName ?: fileName}:\t${result}")
    try {
        Toolkit.getDefaultToolkit().systemClipboard.setContents(StringSelection(result.toString()), null)
        println("Copied to clipboard!\n")
    } catch (_: Exception) {

    }
}

fun <T> measure(func: () -> T): T {
    val start = System.currentTimeMillis()

    return func().also {
        val time = System.currentTimeMillis() - start
        println("Done. Took ${time}ms to run")
    }
}

class Cached2<I1, I2, O>(val func: Cached2<I1, I2, O>.(I1, I2) -> O) {
    private val cache = mutableMapOf<Pair<I1, I2>, O>()

    operator fun invoke(i1: I1, i2: I2): O {
        val key = i1 to i2
        return if (key in cache) {
            cache[key]!!
        } else {
            this.func(i1, i2).also {
                cache[key] = it
            }
        }
    }
}



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
        when(this) {
            RIGHT -> LEFT
            LEFT -> RIGHT
            UP -> DOWN
            DOWN -> UP
        }

    companion object {
        fun fromNSEW(ch: Char) {
            when (ch) {
                'N' -> UP
                'S' -> DOWN
                'E' -> RIGHT
                'W' -> LEFT
            }
        }

        fun fromUDLR(ch: Char) {
            when (ch) {
                'U' -> UP
                'D' -> DOWN
                'R' -> RIGHT
                'L' -> LEFT
            }
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

    companion object {
        val zero = IntVec(0, 0)

        fun fromStr(str: String, delim: String = ","): IntVec {
            val split = str.split(delim)
            return IntVec(split[0].toInt(), split[1].toInt())
        }

        fun String.toIntVec(delim: String = ",")  = fromStr(this, delim)
    }
}

operator fun <E> List<List<E>>.get(intVec: IntVec) = this[intVec.y][intVec.x]
operator fun <E> List<MutableList<E>>.set(intVec: IntVec, value: E) {
    this[intVec.y][intVec.x] = value
}

data class Rectangle(val pos1: IntVec, val pos2: IntVec) {

    operator fun contains(vec: IntVec): Boolean {
        return vec.x in pos1.x..pos2.x && vec.y in pos1.y..pos2.y
    }

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



inline operator fun BigInteger.times(other: Int): BigInteger = this * other.toBigInteger()
inline operator fun BigInteger.plus(other: Int) = this + other.toBigInteger()
inline operator fun BigInteger.minus(other: Int): BigInteger = this - other.toBigInteger()


class Counter<K>(val map: MutableMap<K, BigInteger>) : MutableMap<K, BigInteger> by map {
    constructor() : this(mutableMapOf())

    override operator fun get(key: K): BigInteger {
        return map.getOrDefault(key, BigInteger.ZERO)
    }

    operator fun plus(other: Counter<K>): Counter<K> {
        val newMap = map.toMutableMap()
        other.forEach { (k, v) ->
            newMap.merge(k, v, BigInteger::plus)
        }
        return Counter(newMap)
    }

    operator fun plusAssign(other: Counter<K>) {
        other.forEach { (k, v) ->
            map.merge(k, v, BigInteger::plus)
        }
    }

    operator fun minus(other: Counter<K>): Counter<K> {
        val newMap = map.toMutableMap()
        other.forEach { (k, v) ->
            newMap.merge(k, -v, BigInteger::plus)
        }
        return Counter(newMap)
    }

    fun copy(): Counter<K> {
        return Counter(map.toMutableMap())
    }

    companion object {
        fun letters(str: String): Counter<Char> {
            return fromList(str.toList())
        }

        fun <E> fromList(list: List<E>): Counter<E> {
            return Counter(
                list.groupingBy { it }
                    .eachCount()
                    .mapValues { it.value.toBigInteger() }
                    .toMutableMap()
            )
        }
    }
}


class Parse {
    companion object {
        fun allInts(line: String): List<Int> {
            return """-?\d+""".toRegex().findAll(line)
                .map {
                    it.value.toInt()
                }.toList()
        }

    }
}

fun String.allInts() = Parse.allInts(this)