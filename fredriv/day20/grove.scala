import scala.io.Source

def mix(numbers: Vector[(BigInt, Int)], startIndex: Int): Vector[(BigInt, Int)] =
  val size = numbers.size
  val oldPos = numbers.indexWhere(_._2 == startIndex)
  val numberWithIndex: (BigInt, Int) = numbers(oldPos)

  var newPos = ((oldPos + numberWithIndex._1) % (size - 1)).toInt
  if newPos <= 0 then
    newPos += size - 1

  if newPos == oldPos then
    numbers // no change
  else if newPos < oldPos then
    val start = numbers.take(newPos)
    // new pos
    val middle = numbers.drop(newPos).take(oldPos - newPos)
    // old pos
    val end = numbers.drop(oldPos + 1)
    (start :+ numberWithIndex) ++ middle ++ end
  else // oldPos < newPos
    val start = numbers.take(oldPos)
    // old pos
    val middle = numbers.drop(oldPos + 1).take(newPos - oldPos)
    // new pos
    val end = numbers.drop(newPos + 1)
    start ++ (middle :+ numberWithIndex) ++ end


def mix(numbers: Vector[(BigInt, Int)]): Vector[(BigInt, Int)] =
  (0 until numbers.size).foldLeft(numbers)(mix)


def score(mixed: Vector[(BigInt, Int)]): BigInt =
  // println("Mixed: " + mixed.map(_._1).mkString(", "))
  val size = mixed.size

  val pos = mixed.indexWhere(_._1 == 0)
  val n1 = mixed((pos + 1000) % size)._1
  val n2 = mixed((pos + 2000) % size)._1
  val n3 = mixed((pos + 3000) % size)._1
  println(s"$n1 + $n2 + $n3")
  n1 + n2 + n3


def part1(numbers: Vector[Int]): BigInt =
  val size = numbers.size
  val mixed = mix(numbers.map(BigInt.apply).zipWithIndex)
  score(mixed)


def part2(numbers: Vector[Int]): BigInt =
  val size = numbers.size
  val initial = numbers.map(_ * BigInt("811589153")).zipWithIndex
  val mixed = (1 to 10).foldLeft(initial)((nums, _) => mix(nums))
  score(mixed)


@main def main =
  val input = Source.fromFile("input.txt").getLines.map(_.toInt).toVector

  println(part1(input))
  println(part2(input))
