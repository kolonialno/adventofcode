package com.matsemann.adventofcode2021

import java.math.BigInteger

data class Packet(val version: Int, val type: Int, val value: BigInteger, val subPackets: List<Packet> = listOf())

class Parser(var bits: String) {

    private fun take(nBits: Int) = bits.take(nBits).also { bits = bits.drop(nBits) }

    private fun parseValue(): BigInteger {
        var bitString = ""

        while (bits.startsWith("1")) {
            bitString += take(5).takeLast(4)
        }
        bitString += take(5).takeLast(4)

        return bitString.toBigInteger(2)
    }

    private fun parseSubPackets(type: Int, length: Int): List<Packet> {
        val packets = mutableListOf<Packet>()
        if (type == 1) {
            for (i in 1..length) {
                packets += parsePacket()
            }
        } else {
            val originalLength = bits.length
            while ((originalLength - bits.length) < length) {
                packets += parsePacket()
            }
        }

        return packets
    }

    fun parsePacket(): Packet {
        val version = take(3).toInt(2)
        val id = take(3).toInt(2)

        return if (id == 4) {
            Packet(version, id, parseValue())
        } else {
            val lengthType = take(1).toInt(2)

            val lengthValue: Int = if (lengthType == 0) {
                take(15).toInt(2)
            } else {
                take(11).toInt(2)
            }
            Packet(version, id, BigInteger.ZERO, parseSubPackets(lengthType, lengthValue))
        }

    }

    companion object {
        fun fromLine(line: String) = line.map {
            (it.digitToIntOrNull() ?: (it.code - 'A'.code + 10)).toString(2).padStart(4, '0')
        }.joinToString("") { it }.let {
            Parser(it)
        }
    }

}



fun day16_1(lines: List<String>): Any {
    val parser = Parser.fromLine(lines.first())
    val packet = parser.parsePacket()

    fun traverse(p: Packet): List<Packet> {
        return listOf(p) + p.subPackets.flatMap { traverse(it) }
    }

    val packets = traverse(packet)
    return packets.sumOf { it.version }
}


fun day16_2(lines: List<String>): Any {
    val parser = Parser.fromLine(lines.first())
    val packet = parser.parsePacket()

    fun calculate(p: Packet): BigInteger {
        val subCalculations = p.subPackets.map { calculate(it) }
        when (p.type) {
            0 -> return subCalculations.sumOf { it }
            1 -> return subCalculations.fold(BigInteger.ONE, BigInteger::times)
            2 -> return subCalculations.minOf { it }
            3 -> return subCalculations.maxOf { it }
            4 -> return p.value
            5 -> return subCalculations.let {
                if (it[0] > it[1]) BigInteger.ONE else BigInteger.ZERO
            }
            6 -> return subCalculations.let {
                if (it[0] < it[1]) BigInteger.ONE else BigInteger.ZERO
            }
            7 -> return subCalculations.let {
                if (it[0] == it[1]) BigInteger.ONE else BigInteger.ZERO
            }
            else -> throw Exception("wtf")
        }
    }

    return calculate(packet)
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