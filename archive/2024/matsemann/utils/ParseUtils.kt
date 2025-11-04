package com.matsemann.adventofcode2024.utils


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
fun String.firstInt() = Parse.allIntsInString(this)[0]


fun List<String>.ints(radix: Int = 10) = this.mapNotNull { it.toIntOrNull(radix) }
fun List<String>.bigInts(radix: Int = 10) = this.mapNotNull { it.toBigIntegerOrNull(radix) }
fun List<String>.longs(radix: Int = 10) = this.mapNotNull { it.toLongOrNull(radix) }
