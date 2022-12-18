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


def part1(jets: Array[Char], width: Int, startX: Int, startY: Int): Int =
  val all = ArrayBuffer[Array[Char]]()
  // for i <- 0 to 8 do all.append(Array.fill(width)(' '))

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
    // add new rows to array
    for (row, ry) <- rock.reverse.zipWithIndex
        (c, cx) <- row.zipWithIndex
        if c == '#'
    do
      while y + ry >= all.size do
        // println("Add row to drawing")
        all.append(Array.fill(width)(' '))
      all(y + ry)(x + cx) = '#'


  println("Jets: " + jets.mkString)

  var j = 0

  for i <- 0 until 2022 do
    val rock = rocks(i % rocks.length)
    var y = all.size + startY
    var x = startX
    // println(s"i=$i  j=$j    x=$x y=$y")
    // println("Rock: " + rock.mkString("\n"))

    // move down
    var done = false
    while !done do
      // move left / right
      val jet = jets(j % jets.size)
      // println(s"Jet: $jet")
      if jet == '<' && !collision(rock, x - 1, y) then
        x -= 1
        // println(s"Moving left, x = $x")
      else if jet == '>' && !collision(rock, x + 1, y) then
        x += 1
        // println(s"Moving right, x = $x")

      // move down
      if !collision(rock, x, y - 1) then
        y -= 1
        // println(s"Moving down, y = $y")
      else
        // println("Stopped")
        done = true

      j += 1 // next jet

    drawRock(rock, x, y)

    // for r <- all.reverse do
    //   println("|" + r.mkString + "|")
    // println("+" + ("_" * width) + "+")

  all.size


@main def main =
  val input = Source.fromFile("input.txt").getLines.toList.head

  println("Part 1: " + part1(input.toArray, 7, 2, 3))
