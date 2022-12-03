import scala.io.Source

def priority(c: Char) =
  if c < 'a' then c - 'A' + 27 else c - 'a' + 1

def common(l: List[String]): Char =
  l.map(_.toSet).reduceLeft(_ intersect _).head

@main def main =
  val rucksacks = Source.fromFile("input.txt").getLines.toList
  val containers = rucksacks.map(r => r.splitAt(r.length / 2).toList)
  println("Part 1: " + containers.map(common).map(priority).sum)
  println("Part 2: " + rucksacks.grouped(3).map(common).map(priority).sum)
