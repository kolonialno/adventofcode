package com.matsemann.adventofcode2022

import java.math.BigInteger


class Operation(val expression: String) {
    operator fun invoke(old: BigInteger): BigInteger {
        val parts = expression.split(" ")
        val part1 = if (parts[0] == "old") {
            old
        } else {
            parts[0].toBigInteger()
        }
        val part2 = if (parts[2] == "old") {
            old
        } else {
            parts[2].toBigInteger()
        }
        return when(parts[1]) {
            "*" -> part1 * part2
            else -> part1 + part2
        }
    }
}

data class Monkey(val test:BigInteger, val ifTrue:Int, val ifFalse:Int, val operation: Operation, val items: MutableList<BigInteger>)

fun day11_1(lines: List<String>): Any {
    val monkeys = lines.splitBy { it.isBlank() }.mapIndexed { index, it ->
        Monkey(
            it[3].allInts()[0].big(),
            it[4].allInts()[0],
            it[5].allInts()[0],
            Operation(it[2].substringAfter("= ")),
            it[1].allInts().map { it.big() }.toMutableList()
        )
    }

    val inspections = Counter<Int>()
    repeat(20) {
        monkeys.forEachIndexed { index, monkey ->
            monkey.items.forEach { item ->
                val worry = monkey.operation(item) / 3.big()
                if (worry % monkey.test == 0.big()) {
                    monkeys[monkey.ifTrue].items.add(worry)
                } else {
                    monkeys[monkey.ifFalse].items.add(worry)
                }
                inspections[index]++
            }
            monkey.items.clear()
        }

    }
    println(inspections.map)
    return inspections.values.sortedDescending().take(2).let { it[0] * it[1] }
}


fun day11_2(lines: List<String>): Any {
    val monkeys = lines.splitBy { it.isBlank() }.mapIndexed { index, it ->
        Monkey(
            it[3].allInts()[0].big(),
            it[4].allInts()[0],
            it[5].allInts()[0],
            Operation(it[2].substringAfter("= ")),
            it[1].allInts().map { it.big() }.toMutableList()
        )
    }

    // Make up my own divisor to keep it from growing
    val divisor = monkeys.fold(1.big()) {acc, monkey -> acc * monkey.test }

    val inspections = Counter<Int>()
    repeat(10_000) {
        monkeys.forEachIndexed { index, monkey ->
            monkey.items.forEach { item ->
                val worry = monkey.operation(item) % divisor

                if (worry % monkey.test == 0.big()) {
                    monkeys[monkey.ifTrue].items.add(worry)
                } else {
                    monkeys[monkey.ifFalse].items.add(worry)
                }
                inspections[index]++
            }
            monkey.items.clear()
        }

    }
    println(inspections.map)
    return inspections.values.sortedDescending().take(2).let { it[0] * it[1] }
}

fun main() {

//    run("1", fileName = "day11_ex.txt", func = ::day11_1)
    run("2", fileName = "day11_ex.txt", func = ::day11_2)

//    run("1", fileName = "day11.txt", func = ::day11_1)
    run("2", fileName = "day11.txt", func = ::day11_2)
}

/*
OUTPUT
======


 */