import scala.io.Source
import scala.collection.mutable.ArrayBuffer

case class Pos(x: Int, y: Int):
  override def toString: String = s"($x,$y)"


case class Sensor(pos: Pos, beacon: Pos):
  val beaconDist = manhattan(pos, beacon)
  override def toString: String = s"Sensor($pos beacon:$beacon dist:$beaconDist)"

object Sensor:
  val regex = raw"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)".r
  def parse(line: String): Sensor =
    line match
      case regex(sx, sy, bx, by) => Sensor(Pos(sx.toInt, sy.toInt), Pos(bx.toInt, by.toInt))


case class Line(start: Pos, end: Pos):
  val slope = (end.x - start.x) / (end.y - start.y) // will be 1 or -1

  def contains(p: Pos): Boolean =
    p.x >= Math.min(start.x, end.x) &&
    p.x <= Math.max(start.x, end.x) &&
    p.y >= Math.min(start.y, end.y) &&
    p.y <= Math.max(start.y, end.y)

  def intersectsAt(other: Line): Option[Pos] =
    // println("l1: " + this)
    // println("l2: " + other)
    if slope == other.slope then // parallel
      None
    else
      // y = a * x + b   => b = y - a * x
      val b1 = start.y - slope * start.x
      val b2 = other.start.y - other.slope * other.start.x
      // Find intersection point
      val x = other.slope * (b1 - b2) / 2
      val y = slope * x + b1
      val p = Pos(x, y)
      if contains(p) && other.contains(p) then
        Some(p)
      else
        None


def getBorderLines(s: Sensor): List[Line] =
  val x = s.pos.x
  val y = s.pos.y
  val d = s.beaconDist + 1
  List(
    Line(Pos(x, y - d), Pos(x + d, y)), // UR
    Line(Pos(x, y - d), Pos(x - d, y)), // UL
    Line(Pos(x - d, y), Pos(x, y + d)), // DL
    Line(Pos(x + d, y), Pos(x, y + d))  // DR
  )


def manhattan(from: Pos, to: Pos): Int =
  (from.x - to.x).abs + (from.y - to.y).abs


def combine(ranges: IndexedSeq[Range]): IndexedSeq[Range] =
  val sorted = ranges.sortBy(_.min)
  var result = Vector[Range]()

  var cur = sorted.head
  for r <- sorted.tail do
    if cur.max < r.min - 1 then // gap
      result = result :+ cur
      cur = r
    else if r.max > cur.max then
      cur = cur.min to r.max

  result :+ cur


def covered(sensors: IndexedSeq[Sensor], row: Int): IndexedSeq[Range] =
  for s <- sensors
      yDist = (s.pos.y - row).abs
      if yDist <= s.beaconDist
  yield
    val xDist = s.beaconDist - yDist
    (s.pos.x - xDist) to (s.pos.x + xDist)


def findGap(sensors: IndexedSeq[Sensor], row: Int): Option[Pos] =
  val coveredRanges = combine(covered(sensors, row))
  if coveredRanges.size > 1 then
    Some(Pos(coveredRanges(0).max + 1, row))
  else
    None


def part1(sensors: IndexedSeq[Sensor], row: Int): Int =
  val coveredRanges = combine(covered(sensors, row))

  val beaconsInRanges = sensors
    .map(_.beacon)
    .filter(p => p.y == row && coveredRanges.exists(_.contains(p.x)))
    .toSet

  coveredRanges.map(_.size).sum - beaconsInRanges.size


def part2(sensors: IndexedSeq[Sensor], maxPos: Int): Pos =
  for row <- 0 to maxPos do
    val pos = findGap(sensors, row)
    if pos.nonEmpty then
      return pos.get
  throw Exception("No solution found!")


def intersections(s1: Sensor, s2: Sensor, maxPos: Int): Seq[Pos] =
  for l1 <- getBorderLines(s1)
      l2 <- getBorderLines(s2)
      p <- l1.intersectsAt(l2)
      if p.x >= 0 && p.x <= maxPos && p.y >= 0 && p.y <= maxPos
  yield p


def part2b(sensors: IndexedSeq[Sensor], maxPos: Int): Pos =
  val candidates = 
    for (s1, i) <- sensors.zipWithIndex
        s2 <- sensors.drop(i + 1)
    yield intersections(s1, s2, maxPos)

  for row <- candidates.flatten.map(_.y)
      pos <- findGap(sensors, row)
  do return pos

  throw Exception("No solution found!")


@main def main =
  val data = Source.fromFile("input.txt").getLines.toArray
  val sensors = data.map(Sensor.parse)

  println("Part 1: " + part1(sensors, 2000000))

  val res2 = part2(sensors, 4000000)
  println("Part 2: " + res2 + " = " + (BigInt(res2.x) * 4000000 + res2.y))
  
  val res2b = part2b(sensors, 4000000)
  println("Part 2b: " + res2b + " = " + (BigInt(res2b.x) * 4000000 + res2b.y))
