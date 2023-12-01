package ren.iamka.aoc23.day1

import java.net.URL

fun main(args: Array<String>) {
    val url = Day1::class.java.getResource("/day1/testdata.txt")!!
    val sumPart1 = url.processLines(Day1(part2 = false))
    val sumPart2 = url.processLines(Day1(part2 = true))

    println("Part 1: $sumPart1")
    println("Part 2: $sumPart2")
}

private fun URL.processLines(day1: Day1): Int {
    openStream().use {
        return it?.bufferedReader()!!.lineSequence().map { line ->
            day1.findTwoDigitNumber(line)
        }.sum()
    }
}

class Day1(val part2: Boolean) {
    companion object {
        private val numbers = listOf(
            "one",
            "two",
            "three",
            "four",
            "five",
            "six",
            "seven",
            "eight",
            "nine"
        )
    }

    fun findTwoDigitNumber(line: String): Int {
        return StringBuilder().apply {
            append(findFirstDigit(line))
            append(findLastDigit(line))
        }.toString().toInt()
    }

    private fun findFirstDigit(line: String): Int {
        return (1..line.length).firstNotNullOf { end ->
            findDigit(line = line, start = 0, end = end)
        }
    }

    private fun findLastDigit(line: String): Int {
        return (line.lastIndex downTo 0).firstNotNullOf { start ->
            findDigit(line = line, start = start, end = line.length)
        }
    }

    private fun findDigit(line: String, start: Int, end: Int): Int? {
        val substring = line.substring(start, end)
        (1..9).forEach {
            if (substring.contains(it.toString())) return it
        }
        if (part2) {
            numbers.forEachIndexed { i, number ->
                if (substring.contains(number)) return i + 1
            }
        }
        return null
    }
}