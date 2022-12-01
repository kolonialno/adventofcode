import scala.io.Source

def group(xs: List[String]): List[List[String]] =
    xs match
        case List() => List()
        case "" :: xs1 => group(xs1) // discard empty lines
        case x :: xs1 =>
            val (ys, zs) = xs1 span (!_.isEmpty)
            (x :: ys) :: group(zs)

@main def calories =
    val lines = Source.fromFile("input.txt").getLines.toList
    val caloriesPerElf = group(lines).map(_.map(_.toInt).sum)

    println(caloriesPerElf.max)
    println(caloriesPerElf.sorted(Ordering.Int.reverse).take(3).sum)
