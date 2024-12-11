package com.matsemann.adventofcode2024.utils


class Cached<I1, O>(val func: Cached<I1, O>.(I1) -> O) {
    private val cache = mutableMapOf<I1, O>()

    operator fun invoke(i: I1): O {
        return cache.getOrPut(i) { func(i)}
    }
}

class Cached2<I1, I2, O>(val func: Cached2<I1, I2, O>.(I1, I2) -> O) {
    private val cache = mutableMapOf<Pair<I1, I2>, O>()

    operator fun invoke(i1: I1, i2: I2): O {
        return cache.getOrPut(i1 to i2) { func(i1, i2) }
    }
}