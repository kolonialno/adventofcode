package com.matsemann.adventofcode2021


data class Bingo(
    var value: Int,
    var marked: Boolean
)

data class BingoBoard(
    val nums: Array<Array<Bingo>>
) {

    fun markNum(n: Int) {
        nums.flatten().forEach {
            if (it.value == n) {
                it.marked = true
            }
        }
    }

    fun hasBingo(): Boolean {
        val rows = nums
        if (rows.any { row -> row.all { it.marked } })
            return true

        val cols =
            (0..4).map { i ->
                (0..4).map { j ->
                    nums[j][i]
                }
            }

        if (cols.any { col -> col.all { it.marked } })
            return true

        return false
    }

    fun getScore(): Int {
        return nums.flatten().filterNot { it.marked }.sumOf { it.value }
    }
}

fun day04_1(lines: List<String>): Any {
    val draws = lines[0].split(",").map { it.toInt() }
    val numBoards = (lines.size - 1) / 6

    var boards = mutableListOf<BingoBoard>()
    for (i in 0 until numBoards) {
        val board = BingoBoard(Array(5) { j -> Array(5) { k -> Bingo(0, false) } })
        boards.add(board)

        for (row in 0 until 5) {
            val chunked = lines[i * 6 + row + 2].chunked(3)
            chunked
                .map { it.trim().toInt() }
                .forEachIndexed { index, num ->
                    board.nums[row][index].value = num
                }
        }
    }

    for (draw in draws) {
        boards.forEach { it.markNum(draw) }

        // part 2
        val loserBoards = boards.filterNot { it.hasBingo() }.toMutableList()
        if (loserBoards.size == 0) {
            return boards.first().getScore() * draw
        }
        boards = loserBoards

        // part 1:
//        val winner = boards.firstOrNull { it.hasBingo() }
//        if (winner != null) {
//            return draw * winner.getScore()
//        }
    }

    return "nothing"
}


fun day04_2(lines: List<String>): Any {
    // just modified inline in 1...
    return "dummy"
}

fun main() {
//    run("1", fileName = "day04_ex.txt", func = ::day04_1)
    run("1", fileName = "day04_1.txt", func = ::day04_1)
//    run("2", fileName = "day04_ex.txt", func = ::day04_2)
//    run("2", fileName = "day04_1.txt", func = ::day04_2)
}
