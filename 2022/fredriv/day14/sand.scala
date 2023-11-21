import scala.io.Source

case class Pos(x: Int, y: Int):
  def moveTowards(dest: Pos): Pos =
    if dest.x == x then
      Pos(x, y + (dest.y - y).sign)
    else
      Pos(x + (dest.x - x).sign, y)

object Pos:
  def apply(s: String): Pos =
    val coords = s.split(",").map(_.toInt)
    Pos(coords(0), coords(1))

def drawPath(coords: Array[Array[Char]], from: Pos, to: Pos, minX: Int): Unit =
  var pos = from
  var done = false
  while !done do
    coords(pos.y)(pos.x - minX) = '#'
    if pos == to then done = true else pos = pos.moveTowards(to)

def drawPaths(coords: Array[Array[Char]], paths: List[List[Pos]], minX: Int): Unit =
  for path <- paths do
    for pair <- path.sliding(2) do
      drawPath(coords, pair(0), pair(1), minX)

// Returns whether we can add more sand or not
def addSand(coords: Array[Array[Char]], minX: Int): Boolean =
  val rows = coords.size
  var pos = Pos(500 - minX, 0)
  if coords(pos.y)(pos.x) == 'o' then
    false
  else
    var done = false
    while !done && pos.y < rows - 1 do
      if coords(pos.y + 1)(pos.x) == '.' then
        pos = Pos(pos.x, pos.y + 1)
      else if coords(pos.y + 1)(pos.x - 1) == '.' then
        pos = Pos(pos.x - 1, pos.y + 1)
      else if coords(pos.y + 1)(pos.x + 1) == '.' then
        pos = Pos(pos.x + 1, pos.y + 1)
      else
        coords(pos.y)(pos.x) = 'o'
        done = true
    done

def solve(paths: List[List[Pos]], maxY: Int): Int =
  val cols = paths.flatten.map(_.x)
  val minX = cols.min - 1
  val maxX = cols.max

  val coords = Array.fill(maxY + 1)(Array.fill(maxX - minX + 1)('.'))

  drawPaths(coords, paths, minX)

  var done = false
  var turns = 0
  while !done do
    done = !addSand(coords, minX)
    turns += 1

  turns - 1

def part1(paths: List[List[Pos]]): Unit =
  val maxY = paths.flatten.map(_.y).max
  println(solve(paths, maxY))

def part2(input: List[List[Pos]]): Unit =
  val maxY = input.flatten.map(_.y).max + 2
  val paths = List(Pos(500 - maxY - 1, maxY), Pos(500 + maxY + 1, maxY)) :: input
  println(solve(paths, maxY))

@main def main =
  val data = Source.fromFile("input.txt").getLines.toList
  val paths = for l <- data yield l.split(" -> ").toList.map(Pos.apply)

  part1(paths)
  part2(paths)
