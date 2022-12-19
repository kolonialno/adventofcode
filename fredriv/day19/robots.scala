import scala.io.Source
import Resource._

type Cost = Map[Resource, Int]

case class Blueprint(
  id: Int,
  robotCosts: Map[Resource, Cost]
)

object Blueprint:
  def parse(s: String): Blueprint =
    val regex = raw"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.".r
    s match
      case regex(id, oreOre, clayOre, obsOre, obsClay, geodeOre, geodeObs) =>
        Blueprint(
          id.toInt,
          Map(
            Ore -> Map(Ore -> oreOre.toInt),
            Clay -> Map(Ore -> clayOre.toInt),
            Obsidian -> Map(Ore -> obsOre.toInt, Clay -> obsClay.toInt),
            Geode -> Map(Ore -> geodeOre.toInt, Obsidian -> geodeObs.toInt)
          )
        )

def hasEnoughResources(resources: Map[Resource, Int], required: Map[Resource, Int]): Boolean =
  val enough = for (k, v) <- required yield resources(k) >= v
  !enough.toList.contains(false)

def solve(blueprint: Blueprint, minutes: Int): (Int, Int) =
  val maxResPerTurn: Map[Resource, Int] =
    (for res <- Resource.values 
         if res != Geode
      yield res -> blueprint.robotCosts.values.flatMap(_.get(res)).toList.max).toMap

  println(s"Max resources needed per turn: $maxResPerTurn")

  def dfs(producingRobot: Resource, minute: Int, robots: Map[Resource, Int], resources: Map[Resource, Int], produced: Vector[(Int, Resource)]): (Int, Vector[(Int, Resource)]) =
    val required = blueprint.robotCosts(producingRobot)

    val remainingTime = minutes - minute
    if remainingTime == 0 then
      (resources(Geode) + robots(Geode), produced)
    else if hasEnoughResources(resources, required) then
      val newResources: Map[Resource, Int] = for (k, v) <- resources yield k -> (v + robots(k) - required.getOrElse(k, 0))
      val newRobots = robots + (producingRobot -> (robots(producingRobot) + 1))
      val scores =
        for nextRobot <- Resource.values
            if nextRobot == Geode || newRobots(nextRobot) < maxResPerTurn(nextRobot)
        yield
          dfs(nextRobot, minute + 1, newRobots, newResources, produced :+ (minute, producingRobot))
      if scores.isEmpty then
        (newResources(Geode) + newRobots(Geode) * remainingTime, produced)
      else
        scores.maxBy(_._1)
    else
      val newResources: Map[Resource, Int] = for (k, v) <- resources yield k -> (v + robots(k))
      dfs(producingRobot, minute + 1, robots, newResources, produced)

  val initialRobots: Map[Resource, Int] = Map(Ore -> 1, Clay -> 0, Obsidian -> 0, Geode -> 0)
  val initialResources: Map[Resource, Int] = Map(Ore -> 0, Clay -> 0, Obsidian -> 0, Geode -> 0)

  val start = System.nanoTime
  val alts = for r <- List(Ore, Clay) yield
    dfs(r, 1, initialRobots, initialResources, Vector())
  val (geodes, robots) = alts.maxBy(_._1)
  val end = System.nanoTime
  println(s"${blueprint.id}: $geodes (${(end - start) / 1_000_000} ms)")
  println(s"$robots")
  blueprint.id -> geodes


def solve(blueprints: List[Blueprint], minutes: Int): List[(Int, Int)] =
  for blueprint <- blueprints yield
    solve(blueprint, minutes)


def part1(blueprints: List[Blueprint]): Int =
  solve(blueprints, 24).map { case (id, geodes) => id * geodes}.sum
  

def part2(blueprints: List[Blueprint]): Int =
  solve(blueprints.take(3), 32).map { case (id, geodes) => geodes}.reduce(_ * _)
  

@main def main =
  val input = Source.fromFile("input.txt").getLines.toList
  val blueprints = input.map(Blueprint.parse)

  // for b <- blueprints do
  //   println(b)

  println("Part 1: " + part1(blueprints))
  println("Part 2: " + part2(blueprints))

enum Resource:
  case Ore, Clay, Obsidian, Geode
