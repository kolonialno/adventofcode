package com.matsemann.adventofcode2025.utils

import java.awt.Toolkit
import java.awt.datatransfer.StringSelection
import java.nio.file.Files
import java.nio.file.Path


fun getFileLines(fileName: String) =
    Files.readAllLines(Path.of("adventofcode2025/inputs/$fileName"))

fun run(runName: String? = null, fileName: String, func: (List<String>) -> Any) {
    val lines = getFileLines(fileName)
    val result = measure { func(lines) }

    println("Result for $runName ($fileName):\t${result}")
    copyToClipBoard(result)
}

fun copyToClipBoard(data: Any) {
    try {
        Toolkit.getDefaultToolkit().systemClipboard.setContents(StringSelection(data.toString()), null)
        println("Copied to clipboard!\n")
    } catch (_: Exception) {

    }
}

fun <T> measure(func: () -> T): T {
    val start = System.currentTimeMillis()

    return func().also {
        val time = System.currentTimeMillis() - start
        println("Done. Took ${time}ms to run")
    }
}
