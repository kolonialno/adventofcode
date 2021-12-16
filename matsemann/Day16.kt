package com.matsemann.adventofcode2021

import java.math.BigInteger

data class Packet(val version: Int, val type: Int, val value: BigInteger, val subPackets: List<Packet> = listOf())

fun parseValue(str: String) : Pair<String, BigInteger> {
    var remaining = str
    var bitString = ""

    while(remaining.startsWith("1")) {
        bitString += remaining.drop(1).take(4)
        remaining = remaining.drop(5)
    }
    bitString += remaining.drop(1).take(4)
    remaining = remaining.drop(5)

    return remaining to bitString.toBigInteger(2)
}

fun parseSubPackets(str: String, type: Int, length: Int): Pair<String, List<Packet>> {
    var remaining = str
    val packets = mutableListOf<Packet>()
    if (type == 1) {
        for(i in 1..length) {
            parsePacket(remaining).let {(r, p) ->
                remaining = r
                packets += p
            }
        }
    } else {
        while ((str.length - remaining.length) < length) {
            parsePacket(remaining).let {(r, p) ->
                remaining = r
                packets += p
            }
        }
    }

    return remaining to packets
}

fun parsePacket(str: String): Pair<String, Packet> {
    var remaining = str
    val version = remaining.take(3).toInt(2)
    remaining = remaining.drop(3)

    val id = remaining.take(3).toInt(2)
    remaining = remaining.drop(3)

    if (id == 4) {
        val r = parseValue(remaining)
        return r.first to Packet(version, id, r.second)
    } else {
        val lengthType = remaining.take(1).toInt(2)
        remaining = remaining.drop(1)

        val lengthValue: Int
        if (lengthType == 0) {
            lengthValue = remaining.take(15).toInt(2)
            remaining = remaining.drop(15)
        } else {
            lengthValue = remaining.take(11).toInt(2)
            remaining = remaining.drop(11)
        }
        val r = parseSubPackets(remaining, lengthType, lengthValue)
        return r.first to Packet(version, id, BigInteger.ZERO, r.second)
    }

}
fun day16_1(lines: List<String>): Any {
    val bits = lines.first().map {
        (it.digitToIntOrNull() ?: (it.code - 'A'.code + 10)).toString(2).padStart(4, '0')
    }.joinToString("") { it }

    fun traverse(p: Packet): List<Packet> {
        return listOf(p) + p.subPackets.flatMap { traverse(it) }
    }

    val p = parsePacket(bits)
    val packets = traverse(p.second)

    return packets.sumOf { it.version }
}


fun day16_2(lines: List<String>): Any {
    val bits = lines.first().map {
        (it.digitToIntOrNull() ?: (it.code - 'A'.code + 10)).toString(2).padStart(4, '0')
    }.joinToString("") { it }

    fun calculate(p: Packet): BigInteger {
        when (p.type) {
            0 -> return p.subPackets.sumOf { calculate(it) }
            1 -> return p.subPackets.map { calculate(it) }.fold(BigInteger.ONE, BigInteger::times)
            2 -> return p.subPackets.minOf { calculate(it) }
            3 -> return p.subPackets.maxOf { calculate(it) }
            4 -> return p.value
            5 -> return p.subPackets.map { calculate(it) }.let {
                if (it[0] > it[1]) BigInteger.ONE else BigInteger.ZERO
            }
            6 -> return p.subPackets.map { calculate(it) }.let {
                if (it[0] < it[1]) BigInteger.ONE else BigInteger.ZERO
            }
            7 -> return p.subPackets.map { calculate(it) }.let {
                if (it[0] == it[1]) BigInteger.ONE else BigInteger.ZERO
            }
            else -> throw Exception("wtf")
        }
    }

    val p = parsePacket(bits)
    return calculate(p.second)
}

fun main() {
//    run("1", fileName = "day16_ex.txt", func = ::day16_1)
        run("1", fileName = "day16_1.txt", func = ::day16_1)
//    run("2", fileName = "day16_ex.txt", func = ::day16_2)
    run("2", fileName = "day16_1.txt", func = ::day16_2)
}

/*
OUTPUT
======

Done. Took 2ms to run
Result for 1:	945
Copied to clipboard!

Done. Took 1ms to run
Result for 2:	10637009915279
Copied to clipboard!

 */