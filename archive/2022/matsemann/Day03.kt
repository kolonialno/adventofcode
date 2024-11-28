package com.matsemann.adventofcode2022


fun day03_1(lines: List<String>): Any {
    return lines.map {
        val first = it.take(it.length / 2).toSet()
        val second = it.takeLast(it.length / 2).toSet()

        (first intersect second).first().let {
            if (it.isUpperCase()) {
                it - 'A' + 27
            } else {
                it - 'a' + 1
            }
        }
    }.sum()
}


fun day03_2(lines: List<String>): Any {
    return lines
        .chunked(3)
        .map { it.map(String::toSet)}
        .map {(first, second, third) ->
            (first intersect second intersect third).first().let {
                if (it.isUpperCase()) {
                    it - 'A' + 27
                } else {
                    it - 'a' + 1
                }
            }
        }.sum()
}

fun main() {

//    run("1", fileName = "day03_ex.txt", func = ::day03_1)
//    run("2", fileName = "day03_ex.txt", func = ::day03_2)

    run("1", fileName = "day03.txt", func = ::day03_1)
    run("2", fileName = "day03.txt", func = ::day03_2)
}
