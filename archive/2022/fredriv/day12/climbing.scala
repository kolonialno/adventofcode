import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

case class Dir(x: Int, y: Int):
  override def toString: String = (x, y) match
    case (1, 0)  => "R"
    case (-1, 0) => "L"
    case (0, -1) => "U"
    case (0, 1)  => "D"

object Dir:
  def apply(dir: Char): Dir = dir match
    case 'R' => Dir(1, 0)
    case 'L' => Dir(-1, 0)
    case 'U' => Dir(0, -1)
    case 'D' => Dir(0, 1)

case class Pos(x: Int, y: Int):
  def move(d: Dir): Pos = Pos(x + d.x, y + d.y)


def findAllA(map: Array[Array[Char]]): IndexedSeq[Pos] =
  for y <- 0 until map.length
      row = map(y)
      x <- 0 until row.length
      if row(x) == 'a'
  yield Pos(x, y)

def findStartEnd(map: Array[Array[Char]]): (Pos, Pos) =
  var start: Option[Pos] = None
  var end: Option[Pos] = None

  for y <- 0 until map.length
      row = map(y)
      x <- 0 until row.length
  do
    if row(x) == 'S' then
      start = Some(Pos(x, y))
      row(x) = 'a'
    else if row(x) == 'E' then
      end = Some(Pos(x, y))
      row(x) = 'z'

  (start.get, end.get)


def possibleMoves(map: Array[Array[Char]], pos: Pos): List[Dir] =
  val moves = ArrayBuffer[Dir]()
  val height = map(pos.y)(pos.x)
  val row = map(pos.y)
  if pos.x > 0 && row(pos.x - 1) <= height + 1 then moves.append(Dir('L'))
  if pos.x < row.size - 1 && row(pos.x + 1) <= height + 1 then moves.append(Dir('R'))
  if pos.y > 0 && map(pos.y - 1)(pos.x) <= height + 1 then moves.append(Dir('U'))
  if pos.y < map.size - 1 && map(pos.y + 1)(pos.x) <= height + 1 then moves.append(Dir('D'))
  moves.toList


def shortestPath(map: Array[Array[Char]], start: Pos, end: Pos): Option[Vector[Pos]] =
  val shortest = Map[Pos, Vector[Pos]]()
  var toVisit = Vector[Pos]()

  shortest(start) = Vector()
  toVisit = toVisit ++ possibleMoves(map, start).map(start.move(_))

  var cur = start
  var done = false
  while !done do
    val moves = possibleMoves(map, cur).map(cur.move(_)).filter(p => !shortest.contains(p))
    for pos <- moves do
      shortest(pos) = shortest(cur) :+ pos
      toVisit = toVisit :+ pos

    done = toVisit.isEmpty || toVisit.head == end
    if toVisit.isEmpty then
      done = true
    else
      cur = toVisit.head
      toVisit = toVisit.tail
      done = cur == end

  shortest.get(end)


@main def main =
  val data = Source.fromFile("input.txt").getLines.map(_.toArray).toArray
  val (start, end) = findStartEnd(data)

  println("Part 1: " + shortestPath(data, start, end).get.length)

  val starts = findAllA(data)
  val paths = for s <- starts yield shortestPath(data, s, end)

  println("Part 2: " + paths.filter(_.nonEmpty).map(_.get.length).min)
