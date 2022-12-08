package com.matsemann.adventofcode2022


fun day08_1(lines: List<String>): Any {
    val trees = lines.map { it.split("").filter { it.isNotBlank() }.ints() }

    fun visibles(ints: List<Int>): List<Boolean> {
        var highest = -1
        return ints.map {
            if (it > highest) {
                highest = it
                true
            } else {
                false
            }
        }
    }

    val visibleLeft = trees.map {visibles(it) }
    val visibleRight = trees.map {visibles(it.reversed()).reversed() }

    val transposed = trees.transpose()
    val visibleTop = transposed.map {visibles(it) }.transpose()
    val visibleBottom = transposed.map {visibles(it.reversed()).reversed() }.transpose()


    val mapped = trees.mapIndexed {index, ints ->
        ints.filterIndexed { jindex, i ->
            visibleLeft[index][jindex] || visibleRight[index][jindex]
                    || visibleTop[index][jindex] || visibleBottom[index][jindex]
        }
    }

    return mapped.map { it.size }.sum()
}


fun day08_2(lines: List<String>): Any {
    val trees = lines.map { it.split("").filter {!it.isBlank()}.ints() }

    fun visibles(height: Int, ints: List<Int>): Int {
        val blocked = ints.indexOfFirst { it >= height }
        return if (blocked == -1) {
            ints.size
        } else {
            blocked + 1
        }
    }

    val trs = trees.transpose()
    val scores = trees.mapIndexed { j, row ->
        row.mapIndexed { i, it ->
            val right = visibles(it, row.drop(i + 1))
            val left = visibles(it, row.take(i).reversed())
            val top = visibles(it, trs[i].drop(j+1))
            val bottom = visibles(it, trs[i].take(j).reversed())
            left*right*top*bottom
        }
    }


    return scores.map { it.max() }.max()
}

fun main() {

//    run("1", fileName = "day08_ex.txt", func = ::day08_1)
    run("1", fileName = "day08.txt", func = ::day08_1)

//    run("2", fileName = "day08_ex.txt", func = ::day08_2)
    run("2", fileName = "day08.txt", func = ::day08_2)
}

/*
OUTPUT
======
Done. Took 73ms to run
Result for 1:	1703
Copied to clipboard!

Done. Took 52ms to run
Result for 2:	496650
Copied to clipboard!

 */