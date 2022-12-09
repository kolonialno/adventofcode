import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set

case class Pos(row: Int, col: Int)

def moveKnot(headPos: Pos, tailPos: Pos): Pos =
  val rowDiff = headPos.row - tailPos.row
  val colDiff = headPos.col - tailPos.col

  if rowDiff.abs <= 1 && colDiff.abs <= 1 then
    tailPos // touching
  else if rowDiff == 0 then // horizontal
    Pos(tailPos.row, tailPos.col + (colDiff / colDiff.abs))
  else if colDiff == 0 then // vertical
    Pos(tailPos.row + (rowDiff / rowDiff.abs), tailPos.col)
  else // diagonal
    Pos(tailPos.row + (rowDiff / rowDiff.abs), tailPos.col + (colDiff / colDiff.abs))

def solve(data: List[String], numKnots: Int): Set[Pos] =
  val knots = ArrayBuffer[Pos]()
  for _ <- 1 to numKnots do knots.append(Pos(0, 0))
  val visited = Set(Pos(0, 0))

  for cmd <- data do
    val dir = cmd(0) match
      case 'R' => (0, 1)
      case 'L' => (0, -1)
      case 'U' => (-1, 0)
      case 'D' => (1, 0)

    val len = cmd.drop(2).toInt

    for _ <- 1 to len do
      val headPos = knots(0)
      knots(0) = Pos(headPos.row + dir._1, headPos.col + dir._2)
      for i <- 1 until numKnots do
        knots(i) = moveKnot(knots(i-1), knots(i))
      visited.add(knots(numKnots - 1))

  visited

@main def main =
  val data = Source.fromFile("input.txt").getLines.toList

  val part1 = solve(data, 2)
  println(part1.size)

  val part2 = solve(data, 10)
  println(part2.size)
