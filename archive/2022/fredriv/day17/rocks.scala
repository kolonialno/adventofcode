import scala.io.Source
import scala.collection.mutable.ArrayBuffer

val rocks = Array(
	"""####""".split("\n"),
	""" # 
	  |###
	  | # """.stripMargin.split("\n"),
	"""  #
	  |  #
	  |###""".stripMargin.split("\n"),
	"""#
	  |#
	  |#
	  |#""".stripMargin.split("\n"),
	"""##
	  |##""".stripMargin.split("\n")
)


def solve(jets: Array[Char], maxRocks: Int, width: Int = 7, startX: Int = 2, startY: Int = 3): Int =
  val all = ArrayBuffer[Array[Char]]()

  def collision(rock: Array[String], x: Int, y: Int): Boolean =
    if x < 0 || x + rock(0).size > width || y < 0 then
      true // reached bottom or sides
    else
      val overlaps =
        for (row, ry) <- rock.reverse.zipWithIndex
            if y + ry < all.size
            (c, cx) <- row.zipWithIndex
        yield
          c == '#' && all(y + ry)(x + cx) == '#'

      overlaps.contains(true)

  def drawRock(rock: Array[String], x: Int, y: Int): Unit =
    for (row, ry) <- rock.reverse.zipWithIndex
        (c, cx) <- row.zipWithIndex
        if c == '#'
    do
      while y + ry >= all.size do
        all.append(Array.fill(width)(' '))
      all(y + ry)(x + cx) = '#'

  var j = 0
  var last = 0

  for i <- 0 until maxRocks do
    val rock = rocks(i % rocks.length)
    var y = all.size + startY
    var x = startX

    // move down
    var done = false
    while !done do
      // move left / right
      val jet = jets(j % jets.size)
      if jet == '<' && !collision(rock, x - 1, y) then
        x -= 1
      else if jet == '>' && !collision(rock, x + 1, y) then
        x += 1

      // move down
      if !collision(rock, x, y - 1) then
        y -= 1
      else
        done = true

      j += 1 // next jet

    drawRock(rock, x, y)

  all.size


def part1(jets: Array[Char]): Int =
  solve(jets, 2022)


def part2(jets: Array[Char]): BigInt =
  val goal = BigInt("1000000000000")
  val repeatsAfter = 1745 * jets.size
  val iters = goal / repeatsAfter
  val initial = (goal % repeatsAfter).toInt

  println(s"Computing initial $initial:")
  val r1 = solve(jets, initial)
  println(r1)

  println(s"Computing after first repetition of $repeatsAfter:")
  val r2 = solve(jets, initial + repeatsAfter) - r1
  println(r2)

  iters * r2 + r1


@main def main =
  val input = Source.fromFile("input.txt").getLines.toList.head.toArray

  println("Part 1: " + part1(input))
  println("Part 2: " + part2(input))
