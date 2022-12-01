import scala.io.Source

extension[T](list: List[T])
    def splitBy(delim: T): List[List[T]] =
        list match
            case List() => List()
            case x :: xs1 if x == delim => xs1.splitBy(delim) // discard delimiters
            case x :: xs1 =>
                val (ys, zs) = xs1 span (_ != delim)
                (x :: ys) :: zs.splitBy(delim)

@main def calories =
    val lines = Source.fromFile("input.txt").getLines.toList
    val caloriesPerElf = lines.splitBy("").map(_.map(_.toInt).sum)

    println(caloriesPerElf.max)
    println(caloriesPerElf.sorted(Ordering.Int.reverse).take(3).sum)
