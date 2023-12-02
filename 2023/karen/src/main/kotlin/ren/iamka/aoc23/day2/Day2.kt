package ren.iamka.aoc23.day2

import ren.iamka.aoc23.readLines


fun main(args: Array<String>) {
    val part1 = parse().filter { it.isValid() }.sumOf { it.id }
    println(part1)

    val part2 = parse().sumOf { it.getProduct() }
    println(part2)
}

private fun parse(): List<Game> {
    return "/day2/data.txt".readLines {
        map { line ->
            val (gameId, setString) = line.split(":")
            val id = gameId.split(" ")[1].toInt()
            val sets = setString.split(";")
            val gameSets = sets.map { gameSet ->
                val set = gameSet.split(",")
                val map = set.associate {
                    val (amountString, color) = it.trim().split(" ")
                    val amount = amountString.toInt()
                    color to amount
                }
                GameSet(cubes = map)
            }
            Game(id = id, sets = gameSets)
        }.toList()
    }
}

private fun String.getMaxAmount(): Int {
    return when (this) {
        "red" -> 12
        "green" -> 13
        "blue" -> 14
        else -> throw IllegalArgumentException()
    }
}

private fun Game.isValid(): Boolean {
    sets.forEach { set ->
        set.cubes.forEach { (color, amount) ->
            val maxAmount = color.getMaxAmount()
            if (amount > maxAmount) {
                return false
            }
        }
    }
    return true
}

private fun Game.getProduct(): Int {
val map = mutableMapOf("red" to 0, "green" to 0, "blue" to 0)

    sets.forEach { set ->
        set.cubes.forEach { (color, amount) ->
            map[color] = maxOf(map[color]!!, amount)
        }
    }
    return map["red"]!! * map["green"]!! * map["blue"]!!
}

data class GameSet(val cubes: Map<String, Int>)
data class Game(val id: Int, val sets: List<GameSet>)