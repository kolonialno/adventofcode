package com.matsemann.adventofcode2025

import com.matsemann.adventofcode2025.utils.*


fun day06_1(lines: List<String>): Any {
    return lines.map { line ->
        line.split("\\s+".toRegex()).filter { it.isNotEmpty() }
    }.transpose().sumOf {
        it.dropLast(1).longs().reduce(symbolToLongOp(it.last()))
    }
}


fun day06_2(lines: List<String>): Any {
    val operands = lines.last()
        .split("\\s+".toRegex())
        .filter { it.isNotEmpty() }
        .map(::symbolToLongOp)
    return lines.dropLast(1)
        .transpose()
        .splitBy { it.isBlank() }
        .zip(operands)
        .sumOf { (nums, op) ->
            nums.map { it.trim() }.longs().reduce(op)
        }
}

fun main() {

//    run("1", fileName = "day06_ex.txt", func = ::day06_1)
    run("2", fileName = "day06_ex.txt", func = ::day06_2)


//    run("1", fileName = "day06.txt", func = ::day06_1)
    run("2", fileName = "day06.txt", func = ::day06_2)
}
