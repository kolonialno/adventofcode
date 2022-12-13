import scala.io.Source

case class IntTree(nodes: Vector[Int | IntTree]):
	override def toString = "[" + nodes.mkString(",") + "]"

object IntTree:
	def apply(nodes: (Int | IntTree)*): IntTree = IntTree(Vector(nodes:_*))

case class Pair(left: IntTree, right: IntTree)

def parse(s: String): IntTree =
	var rest = s

	def parseList(): Vector[Int | IntTree] =
		var elems = Vector[Int | IntTree]()
		if rest(0) != '[' then throw Exception("Expected start of list")
		rest = rest.tail

		while rest(0) != ']' do
			if rest(0) == ',' then
				rest = rest.tail
			else if rest(0).isDigit then
				val elem = rest.takeWhile(_.isDigit)
				elems = elems :+ elem.toInt
				rest = rest.drop(elem.length)
			else if rest(0) == '[' then
				val elem = IntTree(parseList():_*)
				elems = elems :+ elem
				rest = rest.tail
			else throw Exception(s"Not handled: $rest")

		elems

	IntTree(parseList():_*)

def compare(left: Vector[Int | IntTree], right: Vector[Int | IntTree]): Option[Boolean] =
	(left, right) match
		case (Vector(), Vector()) => None // undecided, continue to next
		case (_, Vector()) => Some(false)
		case (Vector(), _) => Some(true)
		case (a +: as, b +: bs) =>
			(a, b) match
				case (a: Int, b: Int) =>
					if a < b then Some(true)
					else if a > b then Some(false)
					else compare(as, bs)
				case (a: IntTree, b: IntTree) =>
					compare(a.nodes, b.nodes) orElse compare(as, bs)
				case (a: Int, b: IntTree) =>
				  compare(Vector(a), b.nodes) orElse compare(as, bs)
				case (a: IntTree, b: Int) =>
				  compare(a.nodes, Vector(b)) orElse compare(as, bs)
		case _ => throw Exception(s"Unhandled case: $left, $right")

def compare(p: Pair): Boolean =
	compare(p.left.nodes, p.right.nodes).getOrElse(false)

@main def main =
	val data = Source.fromFile("input.txt").getLines.mkString("\n")

	// Part 1
	val pairs = data.split("\n\n").map(_ split "\n").map { case Array(a, b) => Pair(parse(a), parse(b)) }
	val part1 = for (pair, i) <- pairs.zipWithIndex if compare(pair) yield i + 1
	println(part1.sum)

	// Part 2
	val packets: Vector[IntTree] = data.split("\n").filter(_.nonEmpty).toVector.map(parse(_))
	val markers: Vector[IntTree] = Vector("[[2]]", "[[6]]").map(parse(_))
	val sorted = (packets ++ markers).sortWith { case (a, b) => compare(Pair(a, b)) }

	val pos1 = sorted.indexOf(markers(0)) + 1
	val pos2 = sorted.indexOf(markers(1)) + 1
	println(s"$pos1 * $pos2 = " + (pos1 * pos2))
