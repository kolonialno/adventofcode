package com.matsemann.adventofcode2022

import com.matsemann.adventofcode2022.utils.*

fun day20_1(lines: List<String>): Any {
    // Map with index to handle duplicates
    val nums = lines.mapIndexed { index, line -> index to line.toInt() }

    val result = nums.toMutableList()
    val size = nums.size

    nums.forEach { num ->
        val currentPos = result.indexOf(num)
        var newPos = (num.second + currentPos) % (size -1)
        while (newPos < 1) {
            newPos += size - 1
        }
        result.removeAt(currentPos)
        result.add(newPos, num)

    }

    val indexOfZero = result.indexOfFirst { it.second == 0 }

    return (result[result.negativeIndex(indexOfZero+1000)].second
            + result[result.negativeIndex(indexOfZero+2000)].second
            + result[result.negativeIndex(indexOfZero+3000)].second)

}

fun day20_2(lines: List<String>): Any {
    val nums = lines.mapIndexed { index, line -> index to line.toLong()*811589153 }

    val result = nums.toMutableList()
    val size = nums.size

    repeat(10) {
        nums.forEach { num ->
            val currentPos = result.indexOf(num)
            var newPos = (num.second + currentPos) % (size - 1)
            while (newPos < 1) {
                newPos += size - 1
            }
            result.removeAt(currentPos)
            result.add(newPos.toInt(), num)

        }
    }

    val indexOfZero = result.indexOfFirst { it.second == 0L }


    val second = result[result.negativeIndex(indexOfZero + 1000)].second
    return (second
            + result[result.negativeIndex(indexOfZero+2000)].second
            + result[result.negativeIndex(indexOfZero+3000)].second)
}

fun main() {

//    run("1", fileName = "day20_ex.txt", func = ::day20_1)
    run("2", fileName = "day20_ex.txt", func = ::day20_2)

//    run("1", fileName = "day20.txt", func = ::day20_1)
    run("2", fileName = "day20.txt", func = ::day20_2)
}

/*
OUTPUT
======


 */