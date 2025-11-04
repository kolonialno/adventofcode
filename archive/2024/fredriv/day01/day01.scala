import scala.io.Source
import java.lang.Math

@main def day01 =
    val input = Source.fromFile("input.txt").getLines.toVector
    val Vector(left, right) = input.map(_.split("\\s+").map(_.toInt)).transpose

    val distance = (left.sorted, right.sorted).zipped
        .map((a, b) => Math.abs(a - b))
        .sum
    println("Part 1: " + distance)

    val score = left.map(v => v * right.count(_ == v)).sum
    println("Part 2: " + score)
