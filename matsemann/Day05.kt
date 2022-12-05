package com.matsemann.adventofcode2022


fun day05_1(lines: List<String>): Any {
    val (stacks, moves) = lines.splitBy { it == "" }
    val crates = stacks.last().allInts().size * mutableListOf<Char>()

    stacks.forEach { crt ->
        crt.chunked(4).map { it[1] }.mapIndexed { i, c ->
            if (c != ' ')
                crates[i].add(c)
        }
    }

    moves.map { it.allInts() }
        .forEach { (num, from, to) ->
            for (i in 0 until num) {
                val popped = crates[from - 1].removeFirst()
                crates[to - 1].add(0, popped)
            }
        }

    return crates.map { it.first() }.joinToString("")
}


fun day05_2(lines: List<String>): Any {
    val (stacks, moves) = lines.splitBy { it == "" }
    val crates = stacks.last().allInts().size * mutableListOf<Char>()

    stacks.forEach { crt ->
        crt.chunked(4).map { it[1] }.mapIndexed { i, c ->
            if (c != ' ')
                crates[i].add(c)
        }
    }

    moves.map { it.allInts() }.forEach { (num, from, to) ->
        val toMove = crates[from - 1].take(num)
        crates[from-1] = crates[from-1].drop(num).toMutableList()
        crates[to - 1].addAll(0, toMove)
    }

    return crates.map { it.first() }.joinToString("")
}

fun main() {

    run("1", fileName = "day05_ex.txt", func = ::day05_1)
    run("2", fileName = "day05_ex.txt", func = ::day05_2)

    run("1", fileName = "day05.txt", func = ::day05_1)
    run("2", fileName = "day05.txt", func = ::day05_2)
}
