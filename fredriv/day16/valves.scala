import scala.io.Source

case class Node(name: String, flowRate: Int):
  var neighbours: Vector[Node] = Vector()
  var shortestPaths: Map[Node, Vector[Node]] = Map()

  override def toString: String =
    s"Node($name,$flowRate,[${neighbours.map(_.name).mkString(",")}])"


def parseNodes(lines: List[String]): Map[String, Node] =
  val regex = raw"Valve (\w\w) has flow rate=(\d+); tunnels? leads? to valves? (.+)".r
  val data = 
    for line <- lines yield
      line match
        case regex(name, rate, neighbours) => (name, rate, neighbours)
  val nodes = data.map { case (name, rate, _) => name -> Node(name, rate.toInt) }.toMap
  for (name, _, neighbours) <- data do
    nodes(name).neighbours = neighbours.split(", ").map(nodes(_)).toVector
  nodes


def computeShortestPaths(node: Node): Map[Node, Vector[Node]] =
  var toVisit: Vector[Node] = node.neighbours
  var paths: Map[Node, Vector[Node]] = ((node -> Vector()) +: node.neighbours.map(n => n -> Vector(n))).toMap

  while toVisit.nonEmpty do
    val cur = toVisit.head
    toVisit = toVisit.tail

    for n <- cur.neighbours do
      if !paths.contains(n) then
        val path = paths(cur) :+ n
        paths += n -> path
        toVisit = toVisit :+ n

  paths


sealed trait Step
case class Move(to: Node) extends Step:
  override def toString: String = to.name
case object OpenValve extends Step
case object DoNothing extends Step


def part1(nodes: Map[String, Node], maxTime: Int): (Int, Vector[Step]) =
  val allWithFlow: Set[Node] = nodes.values.filter(_.flowRate > 0).toSet
  val maxFlow = allWithFlow.map(_.flowRate).sum

  var max = 0

  def nextSteps(cur: Node, opened: Set[Node], remaining: Int): Vector[Step] =
    if remaining <= 0 then
      Vector(DoNothing)
    else
      val candidates = ((allWithFlow -- opened) - cur)
        .filter(n => cur.shortestPaths(n).length <= remaining)
      val moves = candidates.toVector.sortBy(_.flowRate).reverse.map(Move.apply)

      val steps =
        if opened.contains(cur) || cur.flowRate == 0 then moves else OpenValve +: moves
      if steps.nonEmpty then steps else Vector(DoNothing)

  def dfs(minutes: Int, cur: Node, steps: Vector[Step], totalFlow: Int, opened: Set[Node]): Option[(Int, Vector[Step])] =
    val flow = opened.map(_.flowRate).sum
    val newTotal = totalFlow + flow
    val remaining = maxTime - minutes

    if newTotal + maxFlow * remaining < max then
      None // cutoff
    else
      // println(s"Path ${steps.mkString("->")}, minute $minutes, flow = $flow, totalFlow = $newTotal")
      val res = for step <- nextSteps(cur, opened, remaining) yield
        step match
          case OpenValve =>
            dfs(minutes + 1, cur, steps :+ step, newTotal, opened + cur)
          case Move(to) =>
            val path = cur.shortestPaths(to)
            val newSteps = steps ++ path.map(Move.apply)
            dfs(minutes + path.length, to, newSteps, newTotal + flow * (path.length - 1), opened)
          case DoNothing =>
            val resultFlow = newTotal + flow * remaining
            if resultFlow > max then
              println(s"Found new max $resultFlow: ${steps.mkString("->")}")
              max = resultFlow
              Some((resultFlow, steps :+ step))
            else
              None
      res.flatten.sortBy { case (flow, steps) => flow }.reverse.headOption

  dfs(1, nodes("AA"), Vector(Move(nodes("AA"))), 0, Set()).get


@main def main =
  val input = Source.fromFile("input.txt").getLines.toList
  val nodes = parseNodes(input)
  
  for n <- nodes.values do
    n.shortestPaths = computeShortestPaths(n)

  val (total, steps) = part1(nodes, 30)
  println(s"Part 1: $total (${steps.mkString("->")})")
