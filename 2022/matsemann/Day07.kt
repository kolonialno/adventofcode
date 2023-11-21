package com.matsemann.adventofcode2022


data class Directory(
    val name: String,
    val parent: Directory?,
    val children: MutableList<Directory> = mutableListOf(),
    var files: Int = 0,
    var calculatedSize: Int = 0
)

fun day07(lines: List<String>): Any {
    val root = Directory("/", parent = null)
    var current = root
    lines.drop(1).forEach {
        if (it.startsWith("$ cd ..")) {
            current = current.parent!!
        } else if (it.startsWith("$ cd")) {
            val path = it.drop(5)
            val newDir = Directory(path, parent = current)
            current.children.add(newDir)
            current = newDir
        } else if (it[0].isDigit()) {
            val size = it.split(" ")[0].toInt()
            current.files += size
        }
    }

    traverse_calculate(root)
    val all = flatten(root)

    val part1 = all.filter { it.calculatedSize < 100000 }.sumOf { it.calculatedSize }

    val currentUsed = all.sumOf { it.files }
    val unused = 70000000 - currentUsed
    val toDelete = 30000000 - unused
    val part2 = all.sortedBy { it.calculatedSize }.first { it.calculatedSize > toDelete }.calculatedSize
    return part1 to part2
}

fun traverse_calculate(directory: Directory): Int {
    val sizes = directory.children.sumOf { traverse_calculate(it) }
    directory.calculatedSize = sizes + directory.files
    return directory.calculatedSize
}

fun flatten(directory: Directory): List<Directory> {
    val children = directory.children.flatMap { flatten(it) }
    return children + directory
}


fun main() {

    run("1", fileName = "day07_ex.txt", func = ::day07)
//    run("2", fileName = "day07_ex.txt", func = ::day07_2)

    run("1", fileName = "day07.txt", func = ::day07)
//    run("2", fileName = "day07.txt", func = ::day07_2)
}

/*
OUTPUT
======
Done. Took 37ms to run
Result for 1:	(95437, 24933642)
Copied to clipboard!

Done. Took 4ms to run
Result for 1:	(1315285, 9847279)
Copied to clipboard!

 */