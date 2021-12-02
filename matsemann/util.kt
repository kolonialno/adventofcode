package com.matsemann.adventofcode2021

import java.nio.file.Files
import java.nio.file.Path


fun getFileLines(fileName: String) =
    Files.readAllLines(Path.of("adventofcode2021/inputs/$fileName"))

fun run(runName: String? = null, fileName: String, func: (List<String>) -> Any) {
    val lines = getFileLines(fileName)
    val result = measure { func(lines) }

    println("Result for ${runName ?: fileName}:\t${result}\n")
}

fun <T> measure(func: () -> T) : T {
    val start = System.currentTimeMillis()

    return func().also {
        val time = System.currentTimeMillis() - start
        println("Done. Took ${time}ms to run")
    }
}
