import scala.io.Source

def priority(c: Char) =
  if c < 'a' then c - 'A' + 27 else c - 'a' + 1

def containers(rucksack: String): List[String] =
  rucksack.splitAt(rucksack.length / 2).toList

def common(l: List[String]): Char =
  l.map(_.toSet).reduceLeft(_ intersect _).head

@main def main =
  val rucksacks = Source.fromFile("input.txt").getLines.toList
  println("Part 1: " + rucksacks.map(containers).map(common).map(priority).sum)
  println("Part 2: " + rucksacks.grouped(3).map(common).map(priority).sum)
