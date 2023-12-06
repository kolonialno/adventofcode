import scala.io.Source

case class Race(duration: Int, record: Long):
    def distance(ms: Int) = BigInt(ms) * (duration - ms)
    def waysToWin =
        var result = 0
        for i <- 1 to duration
            if distance(i) > record
        do result += 1
        result

def parseNumbers(s: String): Vector[Int] =
    "\\d+".r.findAllIn(s).map(_.toInt).toVector

@main def beatrecord =
    val input = Source.fromFile("input.txt").getLines.toVector

    val times = parseNumbers(input(0))
    val distances = parseNumbers(input(1))
    val races = times.zip(distances).map { case (time, distance) => Race(time, distance) }

    val waysToWin = races.map(_.waysToWin)
    println("Part 1: " + waysToWin.reduce(_ * _))

    val totalDuration = races.map(_.duration.toString).reduce(_ + _).toInt
    val totalRecord = races.map(_.record.toString).reduce(_ + _).toLong
    val race = Race(totalDuration, totalRecord)
    println("Part 2: " + race.waysToWin)
