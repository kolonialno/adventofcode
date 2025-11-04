package com.matsemann.adventofcode2024

import com.matsemann.adventofcode2024.utils.*


fun day09_1(lines: List<String>): Any {

    val list = lines.first()
        .map { it.digitToInt() }
        .flatMapIndexed { index, i ->
            if (index % 2 == 0) {
                List(i) { (index / 2).big() }
            } else {
                List(i) { null }
            }
        }.toMutableList()

    var index = 0
    var back = list.size
    while (back > index) {
        var sub = list.subList(index, back)
        val firstSpace = sub.indexOfFirst { it == null }
        val lastData = sub.indexOfLast { it != null }
        if (firstSpace == -1 || lastData == -1) {
            break
        }

        back = index + lastData
        index += firstSpace + 1

        sub[firstSpace] = sub[lastData]
        sub[lastData] = null
    }

    println(list)

    return list.mapIndexed { ind, i -> (i ?: 0.big()) * ind }.sumOf { it }
}


fun day09_2(lines: List<String>): Any {
    data class Data(var index: Int, val size: Int, val file: Int)
    data class Space(var index: Int, var size: Int)

    val datas = mutableListOf<Data>()
    val spaces = mutableListOf<Space>()

    var nextIndex = 0
    lines.first()
        .map { it.digitToInt() }
        .forEachIndexed { index, i ->
            if (index % 2 == 0) {
                datas.add(Data(nextIndex, size = i, file = index / 2))
            } else {
                spaces.add(Space(nextIndex, size = i))
            }
            nextIndex += i
        }


    datas.reversed().forEach { data ->
//      Aww man I misunderstood, I was thinking that for the case 0..1111..
//      it would be possible to move the 1's a bit to the left, and then also having to handle
//      that it now was a new group of 4 spaces.

//        val spaceBefore = spaces.find { it.index + it.size == data.index }
//        val spaceAfter = spaces.find { it.index == data.index + data.size }
//
//        if (spaceBefore != null && spaceAfter != null) {
//            spaceBefore.size += data.size + spaceAfter.size
//            spaceAfter.size = 0
//        } else if (spaceBefore != null) {
//            spaceBefore.size += data.size
//        } else if (spaceAfter != null) {
//            spaceAfter.index -= data.size
//            spaceAfter.size += data.size
//        }

        val space = spaces.firstOrNull { it.size >= data.size && it.index < data.index } ?: return@forEach
        data.index = space.index
        space.size -= data.size
        space.index += data.size

        val overlappingSpace = spaces.find { it.index == space.index + space.size }
        if (overlappingSpace != null) {
            space.size += overlappingSpace.size
            overlappingSpace.size = 0
        }
    }


    return datas.map { data ->
        data to
        ((data.index)..<(data.index+data.size)).sumOf { it.big() * data.file }
        // got so ugly with math since I need a divide by two in my integers
//        (data.index + data.size - 1 + data.index).toBigDecimal() * (((data.size) / 2.0) * data.file).toBigDecimal()
    }
        .sumOf { it.second }

}

fun main() {

//    run("1", fileName = "day09_ex.txt", func = ::day09_1)
//    run("2", fileName = "day09_ex.txt", func = ::day09_2)


//    run("1", fileName = "day09.txt", func = ::day09_1)
    run("2", fileName = "day09.txt", func = ::day09_2)
}
