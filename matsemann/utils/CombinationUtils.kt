package com.matsemann.adventofcode2024.utils

/**
 * A general cartesian function, accepts any number of lists
 * and returns a sequence. So it's lazy, only ever keeping
 * the current product in memory when iterating through it
 *
 * cartesian([1,2], [a,b,c])
 * => [(1,a), (1,b), (1,c), (2,a), (2,b), (2,c)]
 */
fun <E> cartesian(lists: List<List<E>>): Sequence<List<E>> {
    return sequence {
        val counters = Array(lists.size) { 0 }
        val length = lists.fold(1) { acc, list -> acc * list.size }

        for (i in 0 until length) {
            val result = lists.mapIndexed { index, list ->
                list[counters[index]]
            }
            yield(result)
            for (pointer in lists.size - 1 downTo 0) {
                counters[pointer]++
                if (counters[pointer] == lists[pointer].size) {
                    counters[pointer] = 0
                } else {
                    break
                }
            }
        }
    }
}

/**
 * Cartesian product of two lists,
 * util for keeping typing
 */
fun <E, F> cartesian(list1: List<E>, list2: List<F>): Sequence<Pair<E, F>> =
    cartesian(listOf(list1, list2)).map { it[0] as E to it[1] as F }

/**
 * Cartesian product of two lists,
 * util for keeping typing
 */
@JvmName("cartesianExt")
fun <E, F> List<E>.cartesian(list2: List<F>) = cartesian(this, list2)

/**
 * Cartesian product of three lists,
 * util for keeping typing
 */
fun <E, F, G> cartesian(list1: List<E>, list2: List<F>, list3: List<G>): Sequence<Triple<E, F, G>> =
    cartesian(listOf(list1, list2, list3)).map { Triple(it[0] as E, it[1] as F, it[2] as G) }


/**
 * All combinations of items in the list, of the given length.
 * If length is not given, the length is the size of the list
 * Similar to permutations, but order doesn't matter.
 *
 * [a, b, c], length=2 => [(a,b), (a,c), (b, c)]
 * (while permutations would also have (b,a) for instance)
 */
fun <E> combinations(list: List<E>, length: Int? = null): Sequence<List<E>> {

    return sequence {
        val n = list.size
        val r = length ?: list.size
        // a b c d , 2
        val counters = Array(r) { it } // 0,1
        val maxes = Array(r) { it + n - r } // 2 3

        yield(counters.map { list[it] })
        while (true) {
            val lastNotAtMax = counters.indices.findLast { counter ->
                counters[counter] != maxes[counter]
            } ?: return@sequence // All was at max

            counters[lastNotAtMax]++

            // Increase the others behind (that were one max)
            for (toUpdate in lastNotAtMax + 1 until r) {
                counters[toUpdate] = counters[toUpdate - 1] + 1
            }

            yield(counters.map { list[it] })


        }

    }

}

@JvmName("combinationsExt")
fun <E> List<E>.combinations(length: Int? = null) = combinations(this, length)

/**
 * All possible permutations of the list for the given length.
 * If length is not given, the length is the size of the list
 *
 * [a, b, c], length=2 =>
 * [(a,b), (a,c), (b,a), (b,c), (c,a), (c,b)]
 */
fun <E> permutations(list: List<E>, length: Int? = null): Sequence<List<E>> = sequence {
    val n = list.size
    val r = length ?: list.size

    val indices = list.indices.toMutableList()
    val cycles = (n downTo (n - r)).toMutableList()
    yield(indices.take(r).map { list[it] })

    while (true) {
        var broke = false
        for (i in (r - 1) downTo 0) {
            cycles[i]--
            if (cycles[i] == 0) {
                val end = indices[i]
                for (j in i until indices.size - 1) {
                    indices[j] = indices[j + 1]
                }
                indices[indices.size - 1] = end
                cycles[i] = n - i
            } else {
                val j = cycles[i]
                val tmp = indices[i]
                indices[i] = indices[-j + indices.size]
                indices[-j + indices.size] = tmp
                yield(indices.take(r).map { list[it] })
                broke = true
                break
            }
        }
        if (!broke) {
            break
        }
    }
}

operator fun <E> Int.times(list: List<E>): List<List<E>> {
    return List(this) { list }
}

@JvmName("timesMutable")
operator fun <E> Int.times(list: MutableList<E>) =
    MutableList(this) { list.toMutableList() }



fun main() {
    println(permutations(listOf("a", "b", "c")).toList())
    println(permutations(listOf("a", "b", "c"), 2).toList())
    println(permutations(listOf("a", "b", "c", "d"), 2).toList())
    println(permutations(listOf("a", "b", "c", "d"), 2).map { it.toSet() }.toSet())
    println()
    println(combinations(listOf("a", "b", "c", "d"), 2).toList())
    println()
    println(cartesian(2 * listOf("a", "b", "c", "d")).toList())
}