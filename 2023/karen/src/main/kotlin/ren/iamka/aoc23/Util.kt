package ren.iamka.aoc23

import ren.iamka.aoc23.day1.Day1

fun String.readLines(operation: Sequence<String>.() -> Unit) {
    val url = Day1::class.java.getResource(this)!!
    return url.openStream().bufferedReader().useLines(operation)
}