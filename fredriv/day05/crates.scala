import scala.io.Source
import scala.collection.mutable.Stack

def getStacks(stacksData: Array[String]): IndexedSeq[Stack[Char]] =
  val numStacks = (stacksData.head.length + 1) / 4
  val stacks = for _ <- 1 to numStacks yield Stack[Char]()

  for row <- stacksData.tail
      (crate, col) <- row.grouped(4).map(_(1)).zipWithIndex
      if crate != ' '
  do stacks(col).push(crate)

  return stacks

@main def main =
  val data = Source.fromFile("input.txt").getLines.mkString("\n").split("\n\n")
  val stacksData = data(0).split("\n").reverse

  val moves =
    for row <- data(1).split("\n")
        Array(_, num, _, from, _, to) = row.split(" ")
    yield (num.toInt, from.toInt - 1, to.toInt - 1)

  // Part 1:
  val stacks1 = getStacks(stacksData)
  for (num, from, to) <- moves
      _ <- 1 to num
  do stacks1(to).push(stacks1(from).pop())
  println(stacks1.map(_.pop).mkString)

  // Part 2:
  val stacks2 = getStacks(stacksData)
  for (num, from, to) <- moves do {
    val moved = for _ <- 1 to num yield stacks2(from).pop()
    stacks2(to).pushAll(moved.reverse)
  }
  println(stacks2.map(_.pop).mkString)
