package com.matsemann.adventofcode2024.utils


class Cached<I1, O>(val func: Cached<I1, O>.(I1) -> O) {
    private val cache = mutableMapOf<I1, O>()

    operator fun invoke(i: I1): O {
        return if (i in cache) {
            cache[i]!!
        } else {
            this.func(i).also {
                cache[i] = it
            }
        }
    }
}

class Cached2<I1, I2, O>(val func: Cached2<I1, I2, O>.(I1, I2) -> O) {
    private val cache = mutableMapOf<Pair<I1, I2>, O>()

    operator fun invoke(i1: I1, i2: I2): O {
        val key = i1 to i2
        return if (key in cache) {
            cache[key]!!
        } else {
            this.func(i1, i2).also {
                cache[key] = it
            }
        }
    }
}