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