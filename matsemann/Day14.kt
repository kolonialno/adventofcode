package com.matsemann.adventofcode2021

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
    val patterns = mutableMapOf<String, Char>()

    for (i in 2 until lines.size) {
        val a = lines[i].split(" -> ")
        patterns[a[0]] = a[1].first()
    }

    var patternsCount = Counter<String>()
    template.windowed(2).forEach {
        patternsCount[it]++
    }

    for (i in 0 until 40) {
        val new = patternsCount.copy()
        patternsCount.forEach { (pattern, count) ->
            val mapping = patterns[pattern]!!
            new["${pattern[0]}${mapping}"] += count
            new["${mapping}${pattern[1]}"] += count
            new[pattern] -= count
        }
        patternsCount = new
    }

    val lettersCount = Counter<Char>()
    patternsCount.forEach { (pattern, count) ->
        lettersCount[pattern[0]] += count
    }
    lettersCount[template.last()]++

    return lettersCount.values.sorted().let {
        it.last() - it.first()
    }
}

fun day14_jakob(lines: List<String>): Any {
    val template = lines.first()
    val patterns = mutableMapOf<String, Char>()

    for (i in 2 until lines.size) {
        val a = lines[i].split(" -> ")
        patterns[a[0]] = a[1].first()
    }

    val insertions = Cached2<String, Int, Counter<Char>> { str, depth ->
        if (depth == 40) {
            Counter()
        } else {
            val insertion = patterns["${str[0]}${str[1]}"]!!
            val counter = this("${str[0]}$insertion", depth + 1)
            val counter2 = this("$insertion${str[1]}", depth + 1)
            val res = counter + counter2
            res[insertion]++
            res
        }
    }

    val counter = Counter.letters(template)
    template.windowed(2).forEach {
        counter += insertions(it, 0)
    }

    return counter.values.sorted().let {
        it.last() - it.first()
    }
}


fun main() {
//    run("1", fileName = "day14_ex.txt", func = ::day14_1)
//    run("1", fileName = "day14_1.txt",  func = ::day14_1)
//    run("2", fileName = "day14_ex.txt", func = ::day14_2)
//    run("2", fileName = "day14_ex.txt", func = ::day14_3)
    run("2", fileName = "day14_1.txt", func = ::day14_jakob)
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