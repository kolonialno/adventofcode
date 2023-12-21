package com.matsemann.adventofcode2023

import com.matsemann.adventofcode2023.utils.*

fun day19_1(lines: List<String>): Any {
    val (ins, parts) = lines.splitBy { it == "" }
    val workflows = ins.associate { line ->
        val (name, rest) = line.split("{", "}")
        val rules = rest.split(",").map { rule ->
            if (rule.contains(":")) {
                val (variable, value, nextRule) = rule.split(">", "<", ":")
                val gt = rule.contains(">")
                val a =
                    { part: Map<String, Int> -> if (gt && part[variable]!! > value.toInt() || !gt && part[variable]!! < value.toInt()) nextRule else null }
                a
            } else {
                val b = { part: Map<String, Int> -> rule }
                b
            }
        }
        name to rules
    }

    fun checkPart(part: Map<String, Int>, workflow: String): Boolean {
        if (workflow == "R") {
            return false
        } else if (workflow == "A") {
            return true
        }
        val nextWorkFlow = workflows[workflow]!!.firstNotNullOf { rule ->
            rule(part)
        }
        return checkPart(part, nextWorkFlow)
    }

    return parts.map { part ->
        part.drop(1).dropLast(1).split(",").associate { cat ->
            cat.split("=").let { it[0] to it[1].toInt() }
        }
    }.println().filter { part ->
        checkPart(part, "in")
    }.map { part -> part.values.sum() }.sum()

}


fun day19_2(lines: List<String>): Any {
    return 2
}

fun main() {

    run("1", fileName = "day19_ex.txt", func = ::day19_1)
//    run("2", fileName = "day19_ex.txt", func = ::day19_2)


    run("1", fileName = "day19.txt", func = ::day19_1)
//    run("2", fileName = "day19.txt", func = ::day19_2)
}
