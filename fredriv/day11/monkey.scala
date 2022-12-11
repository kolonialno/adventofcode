import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

type WorryLevel = BigInt

case class Operation(operand: Char, x: Int | String)

case class Monkey(
  items: ArrayBuffer[WorryLevel],
  operation: Operation,
  testDivideBy: Int,
  trueTarget: Int,
  falseTarget: Int,
  var inspected: Int = 0
):
  def compute(item: WorryLevel, reduceWorryBy: Int, lcd: Int): WorryLevel =
    val newLevel = operation match
      case Operation('*', "old") => item * item
      case Operation('*', x: Int) => item * x
      case Operation('+', x: Int) => item + x
    (newLevel / reduceWorryBy) % lcd

  def test(item: WorryLevel): Boolean =
    item % testDivideBy == 0

  def inspectItems(reduceWorryBy: Int, lcd: Int): ArrayBuffer[(WorryLevel, Int)] =
    val moves = for item <- items yield
      val newLevel = compute(item, reduceWorryBy, lcd)
      val target = if test(newLevel) then trueTarget else falseTarget
      (newLevel, target)
    inspected += items.size
    items.clear()
    moves

object Monkey:
  def tryParseInt(s: String): Int | String =
    Try(s.toInt).getOrElse(s)

  def apply(data: IndexedSeq[String]): Monkey =
    val items = ArrayBuffer[WorryLevel]()
    items.addAll(data(1).split(": ")(1).split(", ").toList.map(_.toInt).map(BigInt(_)))
    val operation = data(2).split(": ")(1).drop("new = old ".length)
    val op = Operation(operation(0), tryParseInt(operation.drop(2)))
    val testDivideBy = data(3).drop("  Test: divisible by ".length).toInt
    val trueTarget = data(4).last - '0'
    val falseTarget = data(5).last - '0'

    Monkey(items, op, testDivideBy, trueTarget, falseTarget)


def solve(data: String, turns: Int, reduceWorryBy: Int): WorryLevel =
  val monkeys = for d <- data.split("\n\n") yield Monkey(d.split("\n"))
  val lcd = monkeys.map(_.testDivideBy).reduce(_ * _)

  for i <- 1 to turns
      j <- 0 until monkeys.size do
    val moves = monkeys(j).inspectItems(reduceWorryBy, lcd)
    for (item, target) <- moves do monkeys(target).items.append(item)

  val Array(a, b) = monkeys.map(_.inspected).sorted.reverse.take(2).map(BigInt(_))    
  a * b


@main def main =
  val data = Source.fromFile("input.txt").getLines.mkString("\n")

  println("Part 1: " + solve(data, 20, 3))
  println("Part 2: " + solve(data, 10000, 1))
