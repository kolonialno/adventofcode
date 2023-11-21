import scala.io.Source

case class Pos(x: Int, y: Int):
  def +(other: Pos): Pos = Pos(x + other.x, y + other.y)
  override def toString(): String = s"(${x+1},${y+1})"


def parsePath(path: String): Vector[Int | Char] =
  var result = Vector[Int | Char]()
  var p = path
  while p.nonEmpty do
    val n = p.takeWhile(_.isDigit)
    if n.nonEmpty then
      result = result :+ n.toInt
      p = p.drop(n.size)
    else
      result = result :+ p.head
      p = p.tail
  result


def part1(map: Array[Array[Char]], path: Vector[Int | Char]): Int =
  def attemptMove(pos: Pos, facing: Facing): Pos =
    val candidate = facing match
      case Facing.Left =>
        val row = map(pos.y)
        val newX =
          if pos.x > 0 && row(pos.x - 1) != ' ' then
            pos.x - 1
          else // find right edge
            row.lastIndexWhere(_ != ' ')
        Pos(newX, pos.y)

      case Facing.Right =>
        val row = map(pos.y)
        val newX =
          if pos.x < row.size - 1 && row(pos.x + 1) != ' ' then
            pos.x + 1
          else // find left edge
            row.indexWhere(_ != ' ')
        Pos(newX, pos.y)

      case Facing.Up =>
        val newY =
          if pos.y > 0 && map(pos.y - 1)(pos.x) != ' ' then
            pos.y - 1
          else // find bottom edge
            map.lastIndexWhere(row => row(pos.x) != ' ')
        Pos(pos.x, newY)

      case Facing.Down =>
        val newY =
          if pos.y < map.size - 1 && map(pos.y + 1)(pos.x) != ' ' then
            pos.y + 1
          else // find top edge
            map.indexWhere(row => row(pos.x) != ' ')
        Pos(pos.x, newY)

    // print(s"Candidate: $candidate = ")
    val c = map(candidate.y)(candidate.x)
    // println(c)
    if c != '#' then candidate else pos


  def move(pos: Pos, steps: Int, facing: Facing): Pos =
    // println(s"Moving $steps $facing")
    val dir = facing match
      case Facing.Right => Pos(1, 0)
      case Facing.Left => Pos(-1, 0)
      case Facing.Up => Pos(0, -1)
      case Facing.Down => Pos(0, 1)

    var newPos = pos
    for i <- 1 to steps do
      val cand = attemptMove(newPos, facing)
      if cand == newPos then return newPos
      else
        newPos = cand
        map(newPos.y)(newPos.x) = facing.symbol

    newPos


  var pos = Pos(map(0).indexOf('.'), 0)
  // println(s"Starting pos: $pos")
  var facing = Facing.Right
  map(pos.y)(pos.x) = facing.symbol

  for p <- path do
    p match
      case rotate: Char =>
        val d = if rotate == 'L' then -1 else 1
        facing = Facing.fromOrdinal((facing.ordinal + d + 4) % 4)
        // println(s"Turning to $facing")
        map(pos.y)(pos.x) = facing.symbol
      case steps: Int =>
        pos = move(pos, steps, facing)
        // println(s"New pos: $pos")

  map(pos.y)(pos.x) = 'X' // marks the spot
  1000 * (pos.y + 1) + 4 * (pos.x + 1) + facing.ordinal


@main def main =
  val input = Source.fromFile("input.txt").getLines.toVector
  val cols = input.dropRight(2).map(_.length).max
  val map = input.dropRight(2).map(s => s + (" " * (cols - s.length))).map(_.toArray).toArray

  // println("Map:")
  //   for r <- map do println(r.mkString + "|")

  val path = parsePath(input.last)

  // println("Path: " + path)
  println("Part 1: " + part1(map, path))

  // println("Map:")
  //   for r <- map do println(r.mkString)

enum Facing(val symbol: Char):
  case Right extends Facing('>')
  case Down extends Facing('v')
  case Left extends Facing('<')
  case Up extends Facing('^')
