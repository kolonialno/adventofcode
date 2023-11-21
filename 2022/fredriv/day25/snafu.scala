import scala.io.Source

def parseSnafu(s: String): BigInt =
  var res: BigInt = 0
  for c <- s.toArray do
    val i: Int = c match
      case '-' => -1
      case '=' => -2
      case _ => (c - '0').toInt
    res = res * 5 + i
  res


def toSnafu(i: BigInt): String =
  if i == 0 then ""
  else
    val rem: Int = ((i + 2) % 5 - 2).toInt
    val c: Char = rem match
      case -2 => '='
      case -1 => '-'
      case _ => (rem + '0').toChar
    toSnafu((i + 2) / 5) + c


@main def main =
  val input = Source.fromFile("input.txt").getLines.toVector

  val sum = input.map(parseSnafu).reduce(_ + _)

  println("Part 1: " + sum + " -> " + toSnafu(sum))
