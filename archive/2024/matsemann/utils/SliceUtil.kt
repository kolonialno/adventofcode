package com.matsemann.adventofcode2024.utils

import com.matsemann.adventofcode2024.utils.SliceType.*
import com.matsemann.adventofcode2024.utils.SliceType.Companion.end

/*
Not sure if ever should be used, but fun to write..
Allows you to do python style indexing, and even some
numpy style.

How it works is by firstly extending the get method for lists,
allowing one to send in progressions to get out the indexes.

Then some utils for making it possible to write a progession that
goes to the end of the list, without specifying it, using a special
marker called `end`. When that one is encountered in a range, we
instead create our own Slice function to later calculate the end.
 */


class Slice(val start: SliceType, val end: SliceType, val _step: Int = 1) {
    fun toIntProgression(size: Int): IntProgression {
        val progStart = when (start) {
            is Val -> start.value
            End -> size - 1
        }
        val progEnd = when (end) {
            is Val -> end.value
            End -> size - 1
        }

        return IntProgression.fromClosedRange(progStart, progEnd, _step)
    }

    infix fun step(other: Int) = Slice(
        start, end,
        if (_step > 0) other else -other // Keep direction of step
    )

}

sealed class SliceType {
    class Val(val value: Int) : SliceType()
    object End : SliceType()
    companion object {
        val end = End
    }
}

// For doing 5..<end
operator fun Int.rangeUntil(other: End) = Slice(Val(this), other)

// For doing end downTo 5
infix fun End.downTo(other: Int) = Slice(this, Val(other), -1)

// For doing list[1..<end]
operator fun <E> List<E>.get(slice: Slice): List<E> {
    return this[slice.toIntProgression(size)]
}

// For doing list[1..5] or even list[-5..<-1]
operator fun <E> List<E>.get(f: IntProgression): List<E> {
    // Convert negative ranges
    val first = if (f.first < 0) {
        size + f.first
    } else {
        f.first
    }
    val last = if (f.last < 0) {
        size + f.last
    } else {
        f.last
    }
    return slice(IntProgression.fromClosedRange(first, last, f.step))
}

/*
All combinations of slicing or progressing into a 2D list
 */
operator fun <E> List<List<E>>.get(f: IntProgression, b: IntProgression): List<List<E>> {
    return this[f].map { it.slice(b) }
}

operator fun <E> List<List<E>>.get(f: Slice, b: IntProgression): List<List<E>> {
    return this[f].map { it[b] }
}

operator fun <E> List<List<E>>.get(f: IntProgression, b: Slice): List<List<E>> {
    return this[f].map { it[b] }
}

operator fun <E> List<List<E>>.get(f: Slice, b: Slice): List<List<E>> {
    return this[f].map { it[b] }
}

/*
Extract a column from the inner list numpy style.
 */

operator fun <E> List<List<E>>.get(f: Slice, b: Int): List<E> {
    return this[f].map { it[b] }
}

operator fun <E> List<List<E>>.get(f: IntProgression, b: Int): List<E> {
    return this[f].map { it[b] }
}

@OptIn(ExperimentalStdlibApi::class)
fun main() {
    val list1 = (10..30).toList()

    infix fun Any.test(other: Any) {
        println("${this == other} : $this == $other")
    }

    println("1D indexing")
    println("==========")
    // 1: can slice [a..<b]
    // Python: list[a:b] / data[0:1]
    list1[0..<1] test listOf(10)
    list1[0..<1] test list1.take(1)

    list1[1..<3] test listOf(11, 12)
    list1[1..<3] test list1.drop(1).take(2)

    // ..1 instead of ..<1 includes the end
    list1[0..1] test listOf(10, 11)
    list1[1..3] test listOf(11, 12, 13)

    // 2: Can step using infix [a..<b step 2]
    // Python: data[1:6:2]
    list1[1..<6 step 2] test listOf(11, 13, 15)
    list1[1..<7 step 2] test listOf(11, 13, 15)

    // 3: Can avoid specifying end index [a..<end]
    // Python: data[15:]
    list1[0..<end] test (10..30).toList()
    list1[15..<end] test listOf(25, 26, 27, 28, 29, 30)
    list1[15..<end] test list1.drop(15)

    // 4: Reverse / down [a downTo b] (including)
    // or [end downTo b]
    // Python: data[5:1:-1] or data[:16:-1]
    list1[0..<5] test listOf(10, 11, 12, 13, 14)
    list1[5 downTo 1] test listOf(15, 14, 13, 12, 11)
    list1[5 downTo 1] test listOf(15, 14, 13, 12, 11)

    list1[20 downTo 16] test listOf(30, 29, 28, 27, 26)
    list1[end downTo 16] test listOf(30, 29, 28, 27, 26)
    list1[20 downTo 16 step 2] test listOf(30, 28, 26)
    list1[end downTo 16 step 2] test listOf(30, 28, 26)

    println("\nNegative tests")
    // 5: Can index from back using negatives
    // Python: data[-2:20], data[-3:]
    list1[-2..<20] test listOf(29)
    list1[-3..<21] test listOf(28, 29, 30)
    list1[-3..-1] test listOf(28, 29, 30)
    list1[-3..<-1] test listOf(28, 29)
    list1[-3..<end] test listOf(28, 29, 30)

    // Can reverse as well
    // Python: data[-3:-5:-1]
    list1[-5..-3] test listOf(26, 27, 28)
    list1[-3 downTo -5] test listOf(28, 27, 26)

    println("\n2d tests")
    // Can apply to columns as well in nested lists
    // Python numpy: data[3:,1:4:2]
    val list2 = List(5) { i ->
        List(8) { j -> 10 * (i + 1) + j }
    }
    list2[0..<2, 0..<2] test listOf(listOf(10, 11), listOf(20, 21))
    list2[0..<2, 1..<end step 2] test listOf(listOf(11, 13, 15, 17), listOf(21, 23, 25, 27))
    list2[3..<end, 1..<4 step 2] test listOf(listOf(41, 43), listOf(51, 53))

    // Can index into column
    // Python numpy: data[3:,2]
    list2[3..<end, 2] test listOf(42, 52)
}
