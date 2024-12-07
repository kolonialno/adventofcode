package com.matsemann.adventofcode2024

import com.matsemann.adventofcode2024.utils.*


fun day07_1(lines: List<String>): Any {
    val functions = listOf<(Long, Long) -> Long>(Long::plus, Long::times)

    return lines.map {
        it.split(":")
    }.map { (result, others) ->
        result.toLong() to others.split(" ").longs()
    }.filter { (result, numbers) ->
        cartesian((numbers.size - 1) * functions).any {
            numbers.reduceIndexed { index, acc, num ->
                it[index - 1].invoke(acc, num)
            } == result
        }
    }.sumOf { it.first }
}

fun day07_2(lines: List<String>): Any {
    val concat = { l1: Long, l2: Long -> (l1.toString() + l2.toString()).toLong() }
    val functions = listOf<(Long, Long) -> Long>(Long::plus, Long::times, concat)

    return lines.map {
        it.split(":")
    }.map { (result, others) ->
        result.toLong() to others.split(" ").longs()
    }.filter { (result, numbers) ->
        cartesian((numbers.size - 1) * functions).any {
            numbers.reduceIndexed { index, acc, num ->
                it[index - 1].invoke(acc, num)
            } == result
        }
    }.sumOf { it.first }
}

fun main() {

//    run("1", fileName = "day07_ex.txt", func = ::day07_1)
    run("2", fileName = "day07_ex.txt", func = ::day07_2)


//    run("1", fileName = "day07.txt", func = ::day07_1)
    run("2", fileName = "day07.txt", func = ::day07_2)
}
