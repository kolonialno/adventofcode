import scala.io.Source

case class Pos(x: Int, y: Int):
  def +(other: Pos) = Pos(x + other.x, y + other.y)

val surrounding = List(
  Pos(-1,-1), Pos(0,-1), Pos(1,-1),
  Pos(-1,0), Pos(1,0),
  Pos(-1,1), Pos(0,1), Pos(1,1)
)

val toCheck = Map(
  'N' -> List(Pos(0,-1), Pos(1,-1), Pos(-1,-1)),
  'S' -> List(Pos(0,1), Pos(1,1), Pos(-1,1)),
  'W' -> List(Pos(-1,0), Pos(-1,-1), Pos(-1,1)),
  'E' -> List(Pos(1,0), Pos(1,-1), Pos(1,1)),
)

val toDir = Map(
  'N' -> Pos(0,-1),
  'S' -> Pos(0,1),
  'W' -> Pos(-1,0),
  'E' -> Pos(1,0),
)


def parse(input: Vector[Vector[Char]]): Vector[Pos] =
  for (row, y) <- input.zipWithIndex
      (c, x) <- row.zipWithIndex
      if c == '#'
  yield Pos(x, y)


def proposeMove(elf: Pos, occupied: Set[Pos], moves: Vector[Char]): Option[Pos] =
  if surrounding.forall(d => !occupied.contains(elf + d))
    then None
  else
    moves.find(m => toCheck(m).forall(d => !occupied.contains(elf + d))).map(m => elf + toDir(m))


def solve(initialPos: Vector[Pos], initialMoves: Vector[Char], maxRounds: Int): (Int, Vector[Pos]) =
  var elves = initialPos
  var moves = initialMoves

  var round = 0
  while round < maxRounds do
    round += 1
    // println(s"Round: $round")
    val occupied = elves.toSet
    var proposedMoves = for elf <- elves yield proposeMove(elf, occupied, moves)

    var moved = false
    elves = for (newPos, i) <- proposedMoves.zipWithIndex yield
      if newPos.isEmpty || proposedMoves.lastIndexOf(newPos, i - 1) != -1 || proposedMoves.indexOf(newPos, i + 1) != -1 then
        elves(i) // old position
      else
        moved = true
        newPos.get

    if !moved then return (round, elves)

    moves = moves.tail :+ moves.head

  (round, elves)


def part1(initialPos: Vector[Pos], initialMoves: Vector[Char]): BigInt =
  val (rounds, elves) = solve(initialPos, initialMoves, 10)
  val rows = elves.map(_.y).max - elves.map(_.y).min + 1
  val cols = elves.map(_.x).max - elves.map(_.x).min + 1
  rows * cols - elves.size


def part2(initialPos: Vector[Pos], initialMoves: Vector[Char]): Int =
  val (rounds, elves) = solve(initialPos, initialMoves, Int.MaxValue)
  rounds


@main def main =
  val input = Source.fromFile("input.txt").getLines.toVector.map(_.toVector)
  val elves = parse(input)
  val initialMoves = Vector('N', 'S', 'W', 'E')

  println("Part 1: " + part1(elves, initialMoves))

  println("Part 2: " + part2(elves, initialMoves))
