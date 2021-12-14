package com.matsemann.adventofcode2021

import java.math.BigInteger

fun day14_1(lines: List<String>): Any {
    var template = lines.first()
    var changes = mutableMapOf<String, String>()

    for (i in 2 until lines.size) {
        val a = lines[i].split(" -> ")
        changes[a.first()] = a[1]
    }

    for (i in 0 until 10) {
        template = template.windowed(2).map {
            it[0] + changes[it]!!
        }.joinToString("") { it } + template.last()
    }

    return template.groupingBy { it }.eachCount().values.sorted().let {
        it.last() - it.first()
    }
}


fun day14_2(lines: List<String>): Any {
    val template = lines.first()
    val changes = mutableMapOf<String, String>()

    var patternsCount = mutableMapOf<String, BigInteger>()

    for (i in 2 until lines.size) {
        val a = lines[i].split(" -> ")
        changes[a[0]] = a[1]
        patternsCount[a[0]] = 0.toBigInteger()
    }

    template.windowed(2).forEach {
        patternsCount[it] = patternsCount[it]!! + 1
    }


    for (i in 0 until 40) {
        val new = patternsCount.toMutableMap()

        patternsCount.forEach { (pattern, count) ->
            val mapping = changes[pattern]!!
            val new1 = "${pattern[0]}${mapping[0]}"
            val new2 = "${mapping[0]}${pattern[1]}"

            new[new1] = new[new1]!! + count
            new[new2] = new[new2]!! + count
            new[pattern] = new[pattern]!! - count
        }

        patternsCount = new
    }

    val lettersCount = mutableMapOf<Char, BigInteger>()

    patternsCount.forEach { (pattern, count) ->
        lettersCount[pattern[0]] = lettersCount.getOrDefault(pattern[0], 0.toBigInteger()) + count
    }
    lettersCount[template.last()] = lettersCount.getOrDefault(template.last(), 0.toBigInteger()) + 1

    return lettersCount.values.sorted().let {
        it.last() - it.first()
    }
}

fun main() {
//    run("1", fileName = "day14_ex.txt", func = ::day14_1)
    run("1", fileName = "day14_1.txt",  func = ::day14_1)
//    run("2", fileName = "day14_ex.txt", func = ::day14_2)
    run("2", fileName = "day14_1.txt",  func = ::day14_2)
}

/*
OUTPUT
======

Done. Took 3ms to run
Result for 1:	2937
Copied to clipboard!

Done. Took 1ms to run
Result for 2:	3390034818249
Copied to clipboard!

 */