import scala.io.Source

def mix(numbers: Vector[(BigInt, Int)], startIndex: Int): Vector[(BigInt, Int)] =
  val oldPos = numbers.indexWhere(_._2 == startIndex)
  val numberWithIndex: (BigInt, Int) = numbers(oldPos)

  val size = numbers.size - 1 // first and last pos are the same in circular list
  val pos = oldPos + numberWithIndex._1
  val newPos = if pos > 0 then (pos % size).toInt else (pos % size + size).toInt

  val start = numbers.take(Math.min(newPos, oldPos))
  val end = numbers.drop(Math.max(newPos, oldPos) + 1)
  if newPos < oldPos then
    val middle = numbers.drop(newPos).take(oldPos - newPos)
    (start :+ numberWithIndex) ++ middle ++ end
  else
    val middle = numbers.drop(oldPos + 1).take(newPos - oldPos)
    start ++ middle ++ (numberWithIndex +: end)


def mix(numbers: Vector[(BigInt, Int)]): Vector[(BigInt, Int)] =
  (0 until numbers.size).foldLeft(numbers)(mix)


def score(mixed: Vector[(BigInt, Int)]): BigInt =
  val pos = mixed.indexWhere(_._1 == 0)
  val nums = for i <- List(1000, 2000, 3000) yield mixed((pos + i) % mixed.size)._1
  println(nums.mkString(" + "))
  nums.sum


def part1(numbers: Vector[BigInt]): BigInt =
  val mixed = mix(numbers.zipWithIndex)
  score(mixed)


def part2(numbers: Vector[BigInt]): BigInt =
  val initial = numbers.map(_ * 811589153).zipWithIndex
  val mixed = (1 to 10).foldLeft(initial)((nums, _) => mix(nums))
  score(mixed)


@main def main =
  val input = Source.fromFile("input.txt").getLines.map(BigInt.apply).toVector

  println(part1(input))
  println(part2(input))
