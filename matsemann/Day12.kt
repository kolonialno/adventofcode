package com.matsemann.adventofcode2021


data class Node(val name: String, val neighbors: MutableList<Node> = mutableListOf())

fun dfs(start: Node, canVisitSmallTwice: Boolean): Sequence<List<Node>> = sequence {
    suspend fun SequenceScope<List<Node>>.innerDfs(node: Node, path: List<Node>, canVisitSmall: Boolean) {
        if (node.name == "end") {
            yield(path + node)
            return
        }

        var canVisitSmallNew = canVisitSmall
        if (node.name != node.name.uppercase() && node in path) {
            if (node == start || !canVisitSmall) {
                return
            } else {
                canVisitSmallNew = false
            }
        }

        val newPath = path + node
        node.neighbors.forEach { n ->
            innerDfs(n, newPath, canVisitSmallNew)
        }
    }
    innerDfs(start, listOf(), canVisitSmall = canVisitSmallTwice)
}


fun day12(lines: List<String>): Any {

    val nodes = mutableMapOf(
        "start" to Node("start"),
        "end" to Node("end"),
    )

    lines.map { it.split("-") }
        .forEach { (n1, n2) ->
            val node1 = nodes.getOrPut(n1) { Node(n1) }
            val node2 = nodes.getOrPut(n2) { Node(n2) }
            node1.neighbors.add(node2)
            node2.neighbors.add(node1)
        }

//    println("part1: " + dfs(nodes["start"]!!, canVisitSmallTwice = false).count())

    // Sequence emits all paths
    return dfs(nodes["start"]!!, canVisitSmallTwice = true)
        // Can print them as we go
        .onEach { println(it.joinToString { i -> i.name }) }
        // Can take how many you want, it then doesn't waste time computing un-needed stuff
        // This one stops generating when something using a path through KW is found
//        .first { it.any { node -> node.name == "KW" } }.size

        // Comment .first line, and uncomment below for true result
        .count()
}

fun main() {
    for (i in 0..100)
        run("2", fileName = "day12_1.txt", func = ::day12)
}

/*
OUTPUT
======

start, A, c, A, b, A, end
start, A, c, A, b, end
start, A, c, A, end
start, A, b, A, c, A, end
start, A, b, A, end
start, A, b, end
start, A, end
start, b, A, c, A, end
start, b, A, end
start, b, end
Done. Took 1ms to run
Result for 1:	10
Copied to clipboard!

start, A, c, A, c, A, b, A, end
start, A, c, A, c, A, b, end
start, A, c, A, c, A, end
start, A, c, A, b, A, c, A, end
start, A, c, A, b, A, b, A, end
start, A, c, A, b, A, b, end
start, A, c, A, b, A, end
start, A, c, A, b, d, b, A, end
start, A, c, A, b, d, b, end
start, A, c, A, b, end
start, A, c, A, end
start, A, b, A, c, A, c, A, end
start, A, b, A, c, A, b, A, end
start, A, b, A, c, A, b, end
start, A, b, A, c, A, end
start, A, b, A, b, A, c, A, end
start, A, b, A, b, A, end
start, A, b, A, b, end
start, A, b, A, end
start, A, b, d, b, A, c, A, end
start, A, b, d, b, A, end
start, A, b, d, b, end
start, A, b, end
start, A, end
start, b, A, c, A, c, A, end
start, b, A, c, A, b, A, end
start, b, A, c, A, b, end
start, b, A, c, A, end
start, b, A, b, A, c, A, end
start, b, A, b, A, end
start, b, A, b, end
start, b, A, end
start, b, d, b, A, c, A, end
start, b, d, b, A, end
start, b, d, b, end
start, b, end
Done. Took 1ms to run
Result for 2:	36
Copied to clipboard!

 */