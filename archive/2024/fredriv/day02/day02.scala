import scala.io.Source
import java.lang.Math

@main def day02 =
    val input = Source.fromFile("input.txt").getLines.toVector
    val reports = input.map(_.split(" ").map(_.toInt))

    println("Part 1: " + reports.count(isSafe))
    println("Part 2: " + reports.count(isSafeWithDampener))

def isSafe(report: Array[Int]): Boolean =
    val pairs = report.sliding(2).map(p => (p(0), p(1))).toList
    pairs.forall((a, b) => Math.abs(a - b) <= 3) &&
        (pairs.forall((a, b) => a > b) || pairs.forall((a, b) => a < b))

def isSafeWithDampener(report: Array[Int]): Boolean =
    if isSafe(report) then true
    else (0 until report.length).exists { i =>
        val dampened = report.take(i) ++ report.drop(i + 1)
        isSafe(dampened)
    }
