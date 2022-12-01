import scala.io.Source

@main def calories =
  val data = Source.fromFile("input.txt").getLines.mkString("\n")
  val caloriesPerElf =
    for calories <- data.split("\n\n")
    yield calories.split("\n").map(_.toInt).sum

  println(caloriesPerElf.max)
  println(caloriesPerElf.sorted(Ordering.Int.reverse).take(3).sum)
