import scala.io.Source

def regXValues(cmds: List[String]): Array[Int] =
  import scala.collection.mutable.ArrayBuffer

  var regX = 1
  val values = ArrayBuffer(regX)

  for cmd <- cmds do
    if cmd == "noop" then
      values.append(regX)
    else
      values.append(regX).append(regX)
      regX += cmd.drop(5).toInt

  values.toArray


def strengths(values: Array[Int]): Int =
  val strengths =
    for i <- 20 to 220 by 40
      yield i * values(i)
  strengths.sum


def screen(values: Array[Int]): String =
  var screen = Array.fill(6)(Array.fill(40)('.'))

  for row <- 0 until 6
      col <- 0 until 40
  do
    val cycle = row * 40 + col + 1
    val regX = values(cycle)
    val sprite = (regX - 1) to (regX + 1)
    if sprite.contains(col) then
      screen(row)(col) = '#'

  screen.map(row => row.mkString).mkString("\n")


@main def main =
  val cmds = Source.fromFile("input.txt").getLines.toList
  val values = regXValues(cmds)

  println("Part 1: " + strengths(values))
  println("Part 2:\n" + screen(values))
