package com.matsemann.adventofcode2021

fun day02_1(lines: List<String>): Any {
    var x = 0;
    var y = 0;
    lines.forEach {
        val a = it.split(" ")
        val m = a[0]
        val d = a[1].toInt()
        if (m == "forward") {
            x += d
        } else if (m == "down") {
            y += d
        } else if (m == "up") {
            y -= d
        }
    }

    return x * y
}


fun day02_2(lines: List<String>): Any {
    var aim = 0
    var x = 0;
    var y = 0;
    lines.forEach {
        val a = it.split(" ")
        val m = a[0]
        val d = a[1].toInt()
        if (m == "forward") {
            x += d
            y += aim * d
        } else if (m == "down") {
            aim += d
        } else if (m == "up") {
            aim -= d
        }
    }

    return x * y
}

fun main() {
    run("1", fileName = "day02_1.txt", func = ::day02_1)
    run("2", fileName = "day02_1.txt", func = ::day02_2)
}

/*
OUTPUT
======

Done. Took 30ms to run
Result for 1:	1728414

Done. Took 2ms to run
Result for 2:	1765720035

 */