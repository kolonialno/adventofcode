import scala.io.Source

def findUniquePos(data: String, len: Int): Int =
  data.sliding(len).indexWhere(_.toSet.size == len) + len

@main def main =
  val data = Source.fromFile("input.txt").getLines.toList.head

  println("Part 1: " + findUniquePos(data, 4))
  println("Part 2: " + findUniquePos(data, 14))
