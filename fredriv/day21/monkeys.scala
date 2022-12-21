import scala.io.Source

sealed trait Expr
case class Const(c: BigInt) extends Expr
case class Op(a: String, op: Char, b: String) extends Expr


case class Monkey(name: String, expr: Expr)
object Monkey:
  def parse(s: String): Monkey =
    val Array(name, expr) = s.split(": ")
    if expr(0).isDigit then
      Monkey(name, Const(BigInt(expr)))
    else
      val Array(a, op, b) = expr.split(" ")
      Monkey(name, Op(a, op(0), b))


def solve(monkeys: List[Monkey], start: String): BigInt =
  val exprs: Map[String, Expr] = monkeys.map(m => m.name -> m.expr).toMap
  
  def eval(name: String): BigInt =
    exprs(name) match
      case Const(c) => c
      case Op(aName, op, bName) =>
        val a = eval(aName)
        val b = eval(bName)
        op match
          case '+' => a + b
          case '-' => a - b
          case '*' => a * b
          case '/' => a / b

  eval(start)


def part1(monkeys: List[Monkey]): BigInt =
  solve(monkeys, "root")


def part2(monkeys: List[Monkey]): BigInt =
  val exprs: Map[String, Expr] = monkeys.map(m => m.name -> m.expr).toMap
  
  def find(name: String, from: String, path: Vector[String]): Option[Vector[String]] =
    exprs(from) match
      case Const(_) => None
      case Op(a, _, b) if a == name || b == name => Some(path :+ name)
      case Op(a, _, b) => find(name, a, path :+ a) orElse find(name, b, path :+ b)

  def reverse(path: Vector[String], goal: BigInt): BigInt =
    val cur = path(0)
    val next = path(1)
    val Op(aName, op, bName) = exprs(cur).asInstanceOf[Op]

    if aName == next then
      val b = solve(monkeys, bName)
      val a = op match
        case '+' => goal - b   // goal = a + b  =>  a = goal - b
        case '-' => goal + b   // goal = a - b  =>  a = goal + b
        case '*' => goal / b   // goal = a * b  =>  a = goal / b
        case '/' => goal * b   // goal = a / b  =>  a = goal * b
      if next == "humn" then a else reverse(path.tail, a)
    else
      val a = solve(monkeys, aName)
      val b = op match
        case '+' => goal - a   // goal = a + b  =>  b = goal - a
        case '-' => a - goal   // goal = a - b  =>  b = a - goal
        case '*' => goal / a   // goal = a * b  =>  b = goal / a
        case '/' => a / goal   // goal = a / b  =>  b = a / goal
      if next == "humn" then b else reverse(path.tail, b)

  val root = exprs("root").asInstanceOf[Op]

  val path = find("humn", "root", Vector()).get
  if path.head == root.a then
    val goal = solve(monkeys, root.b)
    reverse(path, goal)
  else
    val goal = solve(monkeys, root.a)
    reverse(path, goal)


@main def main =
  val monkeys = Source.fromFile("input.txt").getLines.map(Monkey.parse).toList
  println("Part 1: " + part1(monkeys))
  println("Part 2: " + part2(monkeys))
