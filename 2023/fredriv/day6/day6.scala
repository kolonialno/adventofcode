import scala.io.Source

case class Race(duration: Double, record: Double):
    def distance(ms: Double) = ms * (duration - ms)
    def waysToWin =
        val root = Math.sqrt(duration * duration - 4 * record)
        val max = Math.floor((duration + root) / 2).toLong
        val min = Math.ceil((duration - root) / 2).toLong
        max - min + 1

def parseNumbers(s: String): Vector[Int] =
    "\\d+".r.findAllIn(s).map(_.toInt).toVector

@main def beatrecord =
    val input = Source.fromFile("input.txt").getLines.toVector

    val times = parseNumbers(input(0))
    val distances = parseNumbers(input(1))
    val races = times.zip(distances).map { case (time, distance) => Race(time, distance) }

    val waysToWin = races.map(_.waysToWin)
    println("Part 1: " + waysToWin.reduce(_ * _))

    val totalDuration = races.map(r => f"${r.duration}%1.0f").reduce(_ + _).toInt
    val totalRecord = races.map(r => f"${r.record}%1.0f").reduce(_ + _).toLong
    val race = Race(totalDuration, totalRecord)
    println("Part 2: " + race.waysToWin)
