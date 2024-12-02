package com.matsemann.adventofcode2024

import com.matsemann.adventofcode2024.utils.*
import kotlin.math.abs


fun safe(report: List<Int>) =
    report.windowed(2).all { (a, b) -> abs(a - b) in 1..3 }
            && (report.sorted() == report || report.sorted().reversed() == report)


fun day02_1(lines: List<String>): Any {
    return lines.map { it.allInts() }.count(::safe)
}


fun day02_2(lines: List<String>): Any {
    return lines.map { it.allInts() }
        .count {
            safe(it) || (0..it.size).any { index ->
                safe(it.take(index) + it.drop(index + 1))
            }
        }
}

fun main() {

//    run("1", fileName = "day02_ex.txt", func = ::day02_1)
    run("2", fileName = "day02_ex.txt", func = ::day02_2)


//    run("1", fileName = "day02.txt", func = ::day02_1)
    run("2", fileName = "day02.txt", func = ::day02_2)
}
