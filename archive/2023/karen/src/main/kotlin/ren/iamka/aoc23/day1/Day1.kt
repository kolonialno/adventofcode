package ren.iamka.aoc23.day1

import ren.iamka.aoc23.readLines

fun main() {
    println("Part 1")
    processLines(Day1(part2 = false))
    println("Part 2")
    processLines(Day1(part2 = true))
}

private fun processLines(day1: Day1) {
    "/day1/testdata.txt".readLines {
        val sum = map { line ->
            day1.findTwoDigitNumber(line)
        }.sum()
        println(sum)
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