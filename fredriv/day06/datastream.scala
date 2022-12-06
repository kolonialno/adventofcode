import scala.io.Source

def findUniquePos(data: String, len: Int): Int =
  data.sliding(len, 1)
    .zipWithIndex
    .find((s, _) => s.toSet.size == len)
    .map((_, i) => i + len)
    .get

@main def main =
  val data = Source.fromFile("input.txt").getLines.toList.head

  println("Part 1: " + findUniquePos(data, 4))
  println("Part 2: " + findUniquePos(data, 14))
