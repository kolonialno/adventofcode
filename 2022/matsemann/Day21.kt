package com.matsemann.adventofcode2022

import com.matsemann.adventofcode2022.Expression.Companion.toExpression
import com.matsemann.adventofcode2022.Expression.Constant
import com.matsemann.adventofcode2022.Expression.Operation
import com.matsemann.adventofcode2022.utils.*
import java.math.BigInteger

sealed class Expression(val name: String) {

    class Constant(name: String, val value: BigInteger) : Expression(name)
    class Operation(name: String, val p1: String, val op: String, val p2: String) : Expression(name)

    companion object {
        fun String.toExpression(): Expression {
            val split = this.split(": ")
            return if (split[1][0].isDigit()) {
                Constant(split[0], split[1].toBigInteger())
            } else {
                val split2 = split[1].split(" ")
                Operation(split[0], split2[0], split2[1], split2[2])
            }
        }
    }
}

fun day21_1(lines: List<String>): Any {
    val expressions = lines.map {
        it.toExpression()
    }.associateBy { it.name }.toMutableMap()

    val root = expressions["root"]!!

    fun calculate(expression: Expression): BigInteger {
        when (expression) {
            is Constant -> {
                return expression.value
            }

            is Operation -> {
                val l = calculate(expressions[expression.p1]!!)
                val r = calculate(expressions[expression.p2]!!)
                val result = when (expression.op) {
                    "+" -> l + r
                    "-" -> l - r
                    "*" -> l * r
                    else -> l / r
                }
                expressions[expression.name] = Constant(expression.name, result)
                return result
            }
        }
    }

    return calculate(root)
}


fun day21_2(lines: List<String>): Any {
    val expressions = lines.map { it.toExpression()
    }.associateBy { it.name }.toMutableMap()


    fun calculate(expression: Expression): BigInteger {
        when (expression) {
            is Constant -> {
                return expression.value
            }

            is Operation -> {
                val l = calculate(expressions[expression.p1]!!)
                val r = calculate(expressions[expression.p2]!!)
                val result = when (expression.op) {
                    "+" -> l + r
                    "-" -> l - r
                    "*" -> l * r
                    else -> l / r
                }
//                expressions[expression.name] = Constant(expression.name, result)
                return result
            }
        }
    }


    // A binary search
    // Assumes it's the left expression being affected by humn
    // And for my input increasing humn decreases the res1
    var lowerBound = 1.big()
    var upperBound = Long.MAX_VALUE.toBigInteger()
    var attempt = (lowerBound + upperBound) / 2.big()

    val root =  expressions["root"] as Operation

    while(true) {
        expressions["humn"] = Constant("humn", attempt)
        val res1 = calculate(expressions[root.p1]!!)
        val res2 = calculate(expressions[root.p2]!!)

        if (res1 == res2) {
            return attempt
        }
        if (res1 > res2) { // flip > to < for test input
            lowerBound = attempt
        } else {
            upperBound = attempt
        }

        attempt = (lowerBound + upperBound) / 2.big()

    }


}

fun main() {

//    run("1", fileName = "day21_ex.txt", func = ::day21_1)
//    run("2", fileName = "day21_ex.txt", func = ::day21_2)

//    run("1", fileName = "day21.txt", func = ::day21_1)
    run("2", fileName = "day21.txt", func = ::day21_2)
}

/*
OUTPUT
======


 */