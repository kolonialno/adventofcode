package com.matsemann.adventofcode2021


data class Node(val name:String, val neighbors: MutableList<Node> = mutableListOf())

fun day12_1(lines: List<String>): Any {

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

    var paths = 0

    fun dfs(node:Node, path: List<Node>) {
        if (node.name == "end") {
            println(path.joinToString { it.name } + ", end")
            paths++
            return
        }

        if (node.name != node.name.uppercase() && node in path) {
            return
        }

        val newPath = path + node

        node.neighbors.forEach { n ->
            dfs(n, newPath)
        }
    }

    dfs(nodes["start"]!!, listOf())

    return paths
}


fun day12_2(lines: List<String>): Any {

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

    var paths = 0

    fun dfs(node:Node, path: List<Node>, hasVisitedSmallTwice: Boolean) {
        if (node.name == "end") {
            println(path.joinToString { it.name } + ", end")
            paths++
            return
        }
        var hasVisitedSmallTwiceNew = hasVisitedSmallTwice
        if (node.name != node.name.uppercase() && node in path) {
            if (node.name == "start") {
                return
            }
            if (hasVisitedSmallTwice) {
                return
            } else {
                hasVisitedSmallTwiceNew = true
            }
        }

        val newPath = path + node

        node.neighbors.forEach { n ->
            dfs(n, newPath, hasVisitedSmallTwiceNew)
        }
    }

    dfs(nodes["start"]!!, listOf(), false)

    return paths
}

fun main() {
    run("1", fileName = "day12_ex.txt", func = ::day12_1)
//    run("1", fileName = "day12_1.txt", func = ::day12_1)
    run("2", fileName = "day12_ex.txt", func = ::day12_2)
//    run("2", fileName = "day12_1.txt", func = ::day12_2)
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