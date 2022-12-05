import scala.io.Source
import scala.collection.mutable.Stack

@main def main =
  val data = Source.fromFile("input.txt").getLines.mkString("\n").split("\n\n")
  val stacksData = data(0).split("\n").reverse
  val movesData = data(1).split("\n")

  val numStacks = (stacksData.head.length + 1) / 4
  val stacks = for _ <- 1 to numStacks yield Stack[Char]()

  for row <- stacksData.tail
      col <- 0 until numStacks
      crate = row(col * 4 + 1)
      if crate != ' '
  yield stacks(col).push(crate)

  val moves =
    for row <- movesData
        Array(_, num, _, from, _, to) = row.split(" ")
    yield (num.toInt, from.toInt - 1, to.toInt - 1)

  // Part 1:
  /*
  for (num, from, to) <- moves
      _ <- 1 to num
  yield stacks(to).push(stacks(from).pop())
  */

  // Part 2:
  for (num, from, to) <- moves
  yield {
    val moved = for _ <- 1 to num yield stacks(from).pop()
    stacks(to).pushAll(moved.reverse)
  }

  println(stacks.map(_.pop).mkString)
