package ren.iamka.aoc23.day3

import ren.iamka.aoc23.readLines

fun main() {
    parse {
        getValidNumbers()
    }
}


private fun parse(operation: List<EngineSchematicLine>.() -> Unit) {
    return "/day3/data.txt".readLines {
        map { line ->
            var currentNumberString = ""
            var currentNumberIndex = 0
            val symbols = mutableSetOf<Int>()
            val numbers = mutableMapOf<Int, String>()
            line.forEachIndexed { i, char ->
                // number starts / continues
                if (char.isDigit()) {
                    if (currentNumberString.isEmpty()) currentNumberIndex = i
                    currentNumberString += char
                } // number is done
                else if (currentNumberString.isNotEmpty() && !char.isDigit()) {
                    numbers[currentNumberIndex] = currentNumberString
                    currentNumberString = ""
                }
                // symbol found
                if (char.isSymbol()) {
                    symbols.add(i)
                }
            }
            // add number at the end of the line if exists
            if (currentNumberString.isNotEmpty()) {
                numbers[currentNumberIndex] = currentNumberString
            }
            EngineSchematicLine(symbols = symbols, numbers = numbers)
        }.toList().operation()
    }
}

private fun Char.isDot() = this == '.'
private fun Char.isSymbol() = !isDot() && !isDigit()

private fun List<EngineSchematicLine>.getValidNumbers() {
    var sum = 0
    this.forEachIndexed { i, line ->
        line.numbers.forEach { (j, number) ->
            val validRange = (j - 1..j + number.length).toSet()
            // check previous line
            if (i > 0) {
                if (this[i - 1].symbols.intersect(validRange).isNotEmpty()) {
                    sum += number.toInt()
                    return@forEach
                }
            }
            // check current line
            if (line.symbols.intersect(validRange).isNotEmpty()) {
                sum += number.toInt()
                return@forEach
            }
            // check next line
            if (i < this.lastIndex) {
                if (this[i + 1].symbols.intersect(validRange).isNotEmpty()) {
                    sum += number.toInt()
                    return@forEach
                }
            }
        }
    }
    println(sum)
}


data class EngineSchematicLine(val symbols: Set<Int>, val numbers: Map<Int, String>)