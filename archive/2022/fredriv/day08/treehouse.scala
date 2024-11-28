import scala.io.Source
import Integer.min

def isVisible(data: Array[Array[Int]], row: Int, col: Int): Boolean =
  if row == 0 || col == 0 || row == data.size || col == data(row).size then
    true
  else
    val height = data(row)(col)
    val left = data(row).take(col).forall(_ < height)
    val right = data(row).drop(col + 1).forall(_ < height)
    val up = data.take(row).map(_(col)).forall(_ < height)
    val down = data.drop(row + 1).map(_(col)).forall(_ < height)
    left || right || up || down

def scenicScore(data: Array[Array[Int]], row: Int, col: Int): Int =
  val height = data(row)(col)
  val up = data.take(row).map(_(col)).reverse.takeWhile(_ < height)
  val down = data.drop(row + 1).map(_(col)).takeWhile(_ < height)
  val left = data(row).take(col).reverse.takeWhile(_ < height)
  val right = data(row).drop(col + 1).takeWhile(_ < height)

  min(up.size + 1, row) *
    min(down.size + 1, data.size - row - 1) *
    min(left.size + 1, col) *
    min(right.size + 1, data(row).size - col - 1)

@main def main =
  val data = Source.fromFile("input.txt").getLines.toArray.map(_.toArray.map(_.toInt - '0'))
  val visible = for row <- 0 until data.size
      col <- 0 until data(row).size
      if isVisible(data, row, col)
  yield 1
  println(visible.size)

  val scenicScores = for row <- 0 until data.size
      col <- 0 until data(row).size
  yield scenicScore(data, row, col)
  println(scenicScores.max)
