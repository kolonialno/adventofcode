package ren.iamka.aoc23.day3

import ren.iamka.aoc23.readLines

fun main() {
    parse {
        println(getValidNumbersPart1())
    }

    parse(part2 = true) {
        println(getGearRatiosPart2())
    }
}


private fun parse(part2: Boolean = false, operation: List<EngineSchematicLine>.() -> Unit) {
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
                if ((!part2 && char.isSymbol() || (part2 && char.isGear()))) {
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
private fun Char.isGear() = this == '*'

private fun List<EngineSchematicLine>.getValidNumbersPart1() : Int {
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
    return sum
}

private fun List<EngineSchematicLine>.getGearRatiosPart2(): Int {
    var sum = 0
    this.forEachIndexed { i, line ->
        line.symbols.forEach {
            val adjacentNumbers = mutableListOf<String>()
            // find adjacent numbers to gear symbol
            val validRange = (it - 1..it + 1).toSet()
            // check previous line
            if (i > 0) {
                this[i - 1].numbers.forEach { (j, number) ->
                    val numberRange = (j until j + (number.length)).toSet()
                    if (validRange.intersect(numberRange).isNotEmpty()){
                        adjacentNumbers.add(number)
                    }
                }
            }
            // check current line
            this[i].numbers.forEach { (j, number) ->
                val numberRange = (j until j + (number.length)).toSet()
                if (validRange.intersect(numberRange).isNotEmpty()){
                    adjacentNumbers.add(number)
                }
            }
            // check next line
            if (i < this.lastIndex) {
                this[i + 1].numbers.forEach { (j, number) ->
                    val numberRange = (j until j + (number.length)).toSet()
                    if (validRange.intersect(numberRange).isNotEmpty()){
                        adjacentNumbers.add(number)
                    }
                }
            }

            if (adjacentNumbers.size == 2){
                sum += adjacentNumbers[0].toInt() * adjacentNumbers[1].toInt()
            }
        }
    }
    return sum
}


data class EngineSchematicLine(val symbols: Set<Int>, val numbers: Map<Int, String>)