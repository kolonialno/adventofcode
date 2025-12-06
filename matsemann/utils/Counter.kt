package com.matsemann.adventofcode2025.utils

import java.math.BigInteger
import java.util.*

/**
 * A counter that allows you to count things, similar to the
 * Python variant. Basically a map with a default value so that
 * one can do additions without checking for if it exists or not.
 * Also utils for combining counters or creating from a string or a list.
 */
class Counter<K>(val map: MutableMap<K, BigInteger>) : MutableMap<K, BigInteger> by map {
    constructor() : this(mutableMapOf())

    override operator fun get(key: K): BigInteger {
        return map.getOrDefault(key, BigInteger.ZERO)
    }

    operator fun plus(other: Counter<K>): Counter<K> {
        val newMap = map.toMutableMap()
        other.forEach { (k, v) ->
            newMap.merge(k, v, BigInteger::plus)
        }
        return Counter(newMap)
    }

    operator fun plusAssign(other: Counter<K>) {
        other.forEach { (k, v) ->
            map.merge(k, v, BigInteger::plus)
        }
    }

    operator fun minus(other: Counter<K>): Counter<K> {
        val newMap = map.toMutableMap()
        other.forEach { (k, v) ->
            newMap.merge(k, -v, BigInteger::plus)
        }
        return Counter(newMap)
    }

    fun copy(): Counter<K> {
        return Counter(map.toMutableMap())
    }

    companion object {
        fun Counter<Char>.addLetters(str: String) {
            this += fromLetters(str)
        }

        fun fromLetters(str: String): Counter<Char> {
            return fromList(str.toList())
        }

        fun <E> fromList(list: List<E>): Counter<E> {
            return Counter(
                list.groupingBy { it }
                    .eachCount()
                    .mapValues { it.value.toBigInteger() }
                    .toMutableMap()
            )
        }

    }
}

/**
 * A map where computeIfAbsent is called on each getter
 * Items sorted by key, so safe to use as a sparse list
 */
class DefaultMap<K, V>(val map: MutableMap<K, V> = TreeMap(), val default: (K) -> V) : MutableMap<K, V> by map {

    //    constructor(default: () -> V) : this(mutableMapOf<K, V>(), default)
    override operator fun get(key: K): V {
        return map.computeIfAbsent(key) {
            default(key)
        }
    }
}