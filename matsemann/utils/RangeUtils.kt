package com.matsemann.adventofcode2025.utils

import java.lang.AssertionError
import java.math.BigInteger
import kotlin.math.min
import kotlin.math.max

/**
 * Subtracts the provided range from this one.
 *
 *    |------|
 * -       |----|
 * =  |---|
 *
 * Note that if what's being subtracted lies entirely within
 * the range, two ranges will be returned (the remaining on both sides)
 *
 *   |-----------------|
 * -      |------|
 * = |---|        |----|
 *
 * And opposite, if the range being subtracted is greater, the list
 * will be empty
 */
operator fun IntRange.minus(other: IntRange): List<IntRange> =
    if (!this.overlaps(other)) {
        listOf(this)
    } else {
        val intersection = this.intersect(other)
        listOfNotNull(
            if (this.first < intersection.first) {
                this.first..<intersection.first
            } else {
                null
            },
            if (this.last > intersection.last) {
                (intersection.last + 1)..this.last
            } else {
                null
            }
        )
    }

operator fun LongRange.minus(other: LongRange): List<LongRange> =
    if (!this.overlaps(other)) {
        listOf(this)
    } else {
        val intersection = this.intersect(other)
        listOfNotNull(
            if (this.first < intersection.first) {
                this.first..<intersection.first
            } else {
                null
            },
            if (this.last > intersection.last) {
                (intersection.last + 1)..this.last
            } else {
                null
            }
        )
    }

operator fun ClosedRange<BigInteger>.minus(other: ClosedRange<BigInteger>): List<ClosedRange<BigInteger>> =
    if (!this.overlaps(other)) {
        listOf(this)
    } else {
        val intersection = this.intersect(other)
        listOfNotNull(
            if (this.start < intersection.start) {
                this.start..intersection.start - 1
            } else {
                null
            },
            if (this.endInclusive > intersection.endInclusive) {
                (intersection.endInclusive + 1)..this.endInclusive
            } else {
                null
            }
        )
    }

/**
 * If they share any common values in their ranges
 */
fun IntRange.overlaps(other: IntRange) =
    this.first <= other.last && this.last >= other.first

fun LongRange.overlaps(other: LongRange) =
    this.first <= other.last && this.last >= other.first

fun ClosedRange<BigInteger>.overlaps(other: ClosedRange<BigInteger>) =
    this.start <= other.endInclusive && this.endInclusive >= other.start

/**
 * Keeps only the common parts of the ranges.
 * If no overlap, an empty range is returned
 *
 *    |------------|
 *            |-------|
 *  =         |----|
 */
fun IntRange.intersect(other: IntRange): IntRange =
    if (!this.overlaps(other)) {
        IntRange.EMPTY
    } else {
        val overlapStart = max(this.first, other.first)
        val overlapEnd = min(this.last, other.last)
        overlapStart..overlapEnd
    }

fun LongRange.intersect(other: LongRange): LongRange =
    if (!this.overlaps(other)) {
        LongRange.EMPTY
    } else {
        val overlapStart = max(this.first, other.first)
        val overlapEnd = min(this.last, other.last)
        overlapStart..overlapEnd
    }

fun ClosedRange<BigInteger>.intersect(other: ClosedRange<BigInteger>): ClosedRange<BigInteger> =
    if (!this.overlaps(other)) {
        1.big()..0.big()
    } else {
        val overlapStart = this.start.max(other.start)
        val overlapEnd = this.endInclusive.min(other.endInclusive)
        overlapStart..overlapEnd
    }


/**
 * Joins/unions/merges the two ranges, naively.
 * So make sure they overlap or touches or similar
 * before calling this, or new values can be included
 * in the range
 */
fun IntRange.merge(other: IntRange): IntRange =
    min(this.first, other.first)..max(this.last, other.last)

fun LongRange.merge(other: LongRange): LongRange =
    min(this.first, other.first)..max(this.last, other.last)

fun ClosedRange<BigInteger>.merge(other: ClosedRange<BigInteger>): ClosedRange<BigInteger> =
    (this.start.min(other.start))..(this.endInclusive.max(other.endInclusive))

/**
 * Whether they just touch at one of the ends,
 * not by having the same value, but one range directly follow the other
 * Like [20, 21, 22] and [23, 24...] follows each other
 */
fun IntRange.touches(other: IntRange): Boolean =
    this.last + 1 == other.first || other.last + 1 == this.first

fun LongRange.touches(other: LongRange): Boolean =
    this.last + 1 == other.first || other.last + 1 == this.first

fun ClosedRange<BigInteger>.touches(other: ClosedRange<BigInteger>): Boolean =
    this.endInclusive + 1 == other.start || other.endInclusive + 1 == this.start


//fun <T : Comparable<T>> ClosedRange<T>.overlaps(other: ClosedRange<T>) =
//    this.start <= other.endInclusive && this.endInclusive >= other.start

//fun <T : Comparable<T>> ClosedRange<T>.intersect(other: ClosedRange<T>): ClosedRange<T> {
//    val overlapStart = this.start max other.start
//    val overlapEnd = this.endInclusive min other.endInclusive
//    return overlapStart..overlapEnd
//}


//infix fun <T : Comparable<T>> T.max(other: T): T = if (this > other) this else other
//infix fun <T : Comparable<T>> T.min(other: T): T = if (this < other) this else other


fun main() {

    infix fun <E> E.test(b: E) {
        if (this != b) {
            println()
            println(this)
            println("!= expected:")
            println(b)
            throw AssertionError()
        }
        print("*")
    }

    0..5 test IntRange(0, 5)
    (0..5).toList() test listOf(0, 1, 2, 3, 4, 5)
    (0..<5).toList() test listOf(0, 1, 2, 3, 4)
    10.coerceIn(0..5) test 5

    (15 in (10..20)) test true
    (10..20).overlaps(15..25) test true
    (10..20).overlaps(20..25) test true
    (10..20).overlaps(25..26) test false
    (10..20).overlaps(0..10) test true
    (10..20).overlaps(0..9) test false
    (10..20).overlaps(15..16) test true

    (10..20).intersect(15..25) test (15..20)
    (10..20).intersect(5..25) test (10..20)
    (10..20).intersect(5..15) test (10..15)
    (10..20).intersect(15..17) test (15..17)
    (10..20).intersect(30..40).isEmpty() test true

    (10..20) - (12..18) test listOf(10..11, 19..20)
    (10..20) - (5..25) test listOf()
    (10..20) - (5..15) test listOf(16..20)
    (10..20) - (5..<15) test listOf(15..20)
    (10..20) - (20..30) test listOf(10..19)

    (10..20).touches(20..30) test false
    (10..20).touches(21..30) test true
    (10..20).touches(5..9) test true
    (10..20).touches(5..<10) test true

    (10..20).merge(20..30) test (10..30)
    (10..15).merge(15..30) test (10..30) // be aware
    (10..15).merge(5..11) test (5..15)
    (10..15).merge(5..30) test (5..30)


    (10.big()..20.big()).intersect(15.big()..25.big()) test (15.big()..20.big())
    (10.big()..20.big()).intersect(30.big()..40.big()).isEmpty() test true

}