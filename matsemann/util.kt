package com.matsemann.adventofcode2022

import java.awt.Toolkit
import java.awt.datatransfer.StringSelection
import java.math.BigInteger
import java.nio.file.Files
import java.nio.file.Path
import kotlin.math.*


fun getFileLines(fileName: String) =
    Files.readAllLines(Path.of("adventofcode2022/inputs/$fileName"))

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

class Cached<I1, O>(val func: Cached<I1, O>.(I1) -> O) {
    private val cache = mutableMapOf<I1, O>()

    operator fun invoke(i: I1): O {
        return if (i in cache) {
            cache[i]!!
        } else {
            this.func(i).also {
                cache[i] = it
            }
        }
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
                else -> throw Exception("unkown direction ${ch}")
            }


        fun fromUDLR(ch: Char) =
            when (ch) {
                'U' -> UP
                'D' -> DOWN
                'R', 'F' -> RIGHT
                'L' -> LEFT
                else -> throw Exception("unkown direction ${ch}")

            }



        fun setYDown() {
            DOWN.y = 1
            UP.y = -1
        }
    }
}

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

    companion object {
        val zero = IntVec(0, 0)

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

operator fun <E> List<List<E>>.get(intVec: IntVec) = this[intVec.y][intVec.x]
operator fun <E> List<MutableList<E>>.set(intVec: IntVec, value: E) {
    this[intVec.y][intVec.x] = value
}

fun <E> List<E>.rotate(num: Int): List<E> {
    return if (num >= 0) {
        val n = num % this.size
        this.drop(n) + this.take(n)
    } else {
        val n = -num % this.size
        this.takeLast(n) + this.dropLast(n)
    }
}

operator fun <E> Int.times(list: List<E>): List<List<E>> {
    val lists = Array(this) { list}
    return listOf(*lists)
}

fun <E> permutations(list: List<E>, length: Int? = null): Sequence<List<E>> = sequence {
    val n = list.size
    val r = length ?: list.size

    val indices = list.indices.toMutableList()
    val cycles = (n downTo (n - r)).toMutableList()
    yield(indices.take(r).map { list[it] })

    while (true) {
        var broke = false
        for (i in (r - 1) downTo 0) {
            cycles[i]--
            if (cycles[i] == 0) {
                val end = indices[i]
                for (j in i until indices.size - 1) {
                    indices[j] = indices[j + 1]
                }
                indices[indices.size - 1] = end
                cycles[i] = n - i
            } else {
                val j = cycles[i]
                val tmp = indices[i]
                indices[i] = indices[-j + indices.size]
                indices[-j + indices.size] = tmp
                yield(indices.take(r).map { list[it] })
                broke = true
                break
            }
        }
        if (!broke) {
            break
        }
    }
}

fun <E, F> cartesian(list1: List<E>, list2: List<F>): Sequence<Pair<E, F>> =
    cartesian(listOf(list1, list2)).map { it[0] as E to it[1] as F }

fun <E, F, G> cartesian(list1: List<E>, list2: List<F>, list3: List<G>): Sequence<Triple<E, F, G>> =
    cartesian(listOf(list1, list2, list3)).map { Triple(it[0] as E, it[1] as F, it[2] as G) }

fun <E> cartesian(lists: List<List<E>>): Sequence<List<E>> {
    return sequence {
        val counters = Array(lists.size) { 0 }
        val length = lists.fold(1) { acc, list -> acc * list.size }

        for (i in 0 until length) {
            val result = lists.mapIndexed { index, list ->
                list[counters[index]]
            }
            yield(result)
            for (pointer in lists.size - 1 downTo 0) {
                counters[pointer]++
                if (counters[pointer] == lists[pointer].size) {
                    counters[pointer] = 0
                } else {
                    break
                }
            }
        }
    }
}


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
        fun Counter<Char>.addLetters(str: String) {
            this += fromLetters(str)
        }

        fun fromLetters(str: String): Counter<Char> {
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
        fun allIntsInString(line: String): List<Int> {
            return """-?\d+""".toRegex().findAll(line)
                .map { it.value.toInt() }
                .toList()
        }

    }
}

fun String.allInts() = Parse.allIntsInString(this)


interface Graph {

}

class DirectedGraph() : Graph {

    fun addEdge() {
        // Updates if existing
    }

    fun addMultiEdge() {
        // Allows more between same
    }

    fun addEdgeBothWays() {

    }

    fun addNode() {

    }

    fun convertToAdjacentGraph() {
        // ?
    }
}

class Dijkstra() {}
class AStar() {}
class FloydWarshall() {}


val fullBlock = '█'