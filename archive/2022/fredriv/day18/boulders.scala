import scala.io.Source

case class Pos(x: Int, y: Int, z: Int)

val sides = List(Pos(1,0,0), Pos(0,1,0), Pos(0,0,1), Pos(-1,0,0), Pos(0,-1,0), Pos(0,0,-1))

def exposedSides(cube: Pos, f: Pos => Boolean): Int =
  var exposed = 0
  for d <- sides
  do
    val other = Pos(cube.x + d.x, cube.y + d.y, cube.z + d.z)
    if f(other) then
      exposed += 1
  exposed

def part1(cubes: Set[Pos]): Int =
  cubes.toList.map(cube => exposedSides(cube, p => !cubes.contains(p))).sum


def part2(cubes: Set[Pos]): Int =
  var water = Set[Pos]()
  val max = Pos(cubes.map(_.x).max + 1, cubes.map(_.y).max + 1, cubes.map(_.z).max + 1)

  def expandWater(startPos: Pos): Unit =
    var toVisit = Vector(startPos)
    while toVisit.nonEmpty do
      val p = toVisit.head
      toVisit = toVisit.tail

      if !water.contains(p) && !cubes.contains(p) &&
          p.x >= -1 && p.x <= max.x &&
          p.y >= -1 && p.y <= max.y &&
          p.z >= -1 && p.z <= max.z
      then
        water += p
        for d <- sides
        do
          toVisit = toVisit :+ Pos(p.x + d.x, p.y + d.y, p.z + d.z)

  expandWater(max)

  cubes.toList.map(cube => exposedSides(cube, p => water.contains(p))).sum


@main def main =
  val input = Source.fromFile("input.txt").getLines.toList.map(_.split(","))

  val cubes = input.map { case Array(x, y, z) => Pos(x.toInt, y.toInt, z.toInt) }.toSet

  println("Part 1: " + part1(cubes))
  println("Part 2: " + part2(cubes))
