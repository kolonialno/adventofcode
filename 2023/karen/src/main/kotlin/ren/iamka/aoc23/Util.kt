package ren.iamka.aoc23

import ren.iamka.aoc23.day1.Day1

fun <R> String.readLines(operation: Sequence<String>.() -> R): R {
    val url = Day1::class.java.getResource(this)!!
    url.openStream().use {
        return it?.bufferedReader()!!.lineSequence().operation()
    }
}