package com.matsemann.adventofcode2022

import com.matsemann.adventofcode2022.Packets.PacketList
import com.matsemann.adventofcode2022.Packets.PacketValue
import com.matsemann.adventofcode2022.utils.*
import java.lang.Exception


sealed class Packets {
    data class PacketList(val packets: List<Packets>) : Packets()
    data class PacketValue(val packets: Int) : Packets()
}

fun compare(p1: Packets, p2: Packets): Int {
    return when {
        p1 is PacketValue && p2 is PacketValue ->
            (p1.packets.compareTo(p2.packets))

        p1 is PacketList && p2 is PacketList -> {
            compare(p1, p2)
        }

        p1 is PacketList && p2 is PacketValue -> {
            compare(p1, PacketList(listOf(p2)))
        }

        p1 is PacketValue && p2 is PacketList -> {
            compare(PacketList(listOf(p1)), p2)
        }

        else -> {
            throw Exception("wtf")
        }
    }
}

fun compare(p1: PacketList, p2: PacketList): Int {
    val pairs = p1.packets.zip(p2.packets)
    for ((el1, el2) in pairs) {
        val res = compare(el1, el2)
        if (res != 0) {
            return res
        }
    }
    if (p1.packets.size == p2.packets.size) {
        return 0
    } else if (p1.packets.size < p2.packets.size) {
        return -1
    } else {
        return 1
    }
}

fun parse(str: String): Packets? {
    if (str.isEmpty()) {
        return null
    }
    if (str[0].isDigit()) {
        return PacketValue(str.toInt())
    }

    var bracketCount = 0
    var lastComma = 0

    val packets = mutableListOf<Packets?>()

    for ((i, c) in str.withIndex()) {
        if (c == '[') {
            bracketCount++
        } else if (c == ']') {
            bracketCount--
            if (bracketCount == 0) {
                packets += parse(str.take(i).drop(lastComma + 1))
            }
        }
        if (c == ',') {
            if (bracketCount == 1) {
                packets += parse(str.take(i).drop(lastComma + 1))
                lastComma = i
            }
        }
    }

    return PacketList(packets.filterNotNull())
}

fun day13_1(lines: List<String>): Any {
    return lines.splitBy { it.isEmpty() }.map { (l1, l2) ->
        parse(l1)!! to parse(l2)!!
    }.map { (p1, p2) ->
        val res = compare(p1, p2)
        res
    }.mapIndexed { i, res -> i to res }
        .filter { (i, res) -> res == -1 }
        .sumOf { (i, res) -> i + 1 }
}

fun day13_2(lines: List<String>): Any {
    val div1 = PacketList(listOf(PacketList(listOf(PacketValue(2)))))
    val div2 = PacketList(listOf(PacketList(listOf(PacketValue(6)))))

    val packets = lines.splitBy { it.isEmpty() }.flatMap { (l1, l2) ->
        listOf(parse(l1)!!, parse(l2)!!)
    } + div1 + div2

    val sorted = packets.sortedWith { p1, p2 -> compare(p1, p2) }

    val index1 = sorted.indexOf(div1)
    val index2 = sorted.indexOf(div2)

    return (index1 + 1) * (index2 + 1)
}

fun main() {

//    run("1", fileName = "day13_ex.txt", func = ::day13_1)
//    run("1", fileName = "day13.txt", func = ::day13_1)

    run("2", fileName = "day13_ex.txt", func = ::day13_2)
    run("2", fileName = "day13.txt", func = ::day13_2)
}

/*
OUTPUT
======


 */