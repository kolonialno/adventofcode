import scala.io.Source
import scala.collection.mutable.ArrayBuffer

case class Dir(
  name: String,
  parent: Option[Dir] = None,
  subdirs: ArrayBuffer[Dir] = ArrayBuffer(),
  var totalFileSize: Int = 0)
{
  lazy val size: Int = subdirs.map(_.size).sum + totalFileSize
  override def toString: String = s"Dir($name, $totalFileSize, [${subdirs.map(_.toString).mkString(", ")}])"
}

def buildTree(data: List[String]): Dir =
  val root = Dir("/")
  var currentDir = root
  for line <- data.tail do // skip first "$ cd /"
    if line == "$ cd .." then
      currentDir = currentDir.parent.get
    else if line.startsWith("$ cd ") then
      val name = line.drop(5)
      val dir = Dir(name, Some(currentDir))
      currentDir.subdirs.append(dir)
      currentDir = dir
    else if line(0).isDigit then
      val size = line.split(" ")(0).toInt
      currentDir.totalFileSize += size
  root

def findDirs(dir: Dir, cond: Dir => Boolean): ArrayBuffer[Dir] =
  val children = dir.subdirs.flatMap(findDirs(_, cond))
  if cond(dir) then dir +: children else children

@main def main =
  val data = Source.fromFile("input.txt").getLines.toList
  val root = buildTree(data)

  val part1 = findDirs(root, d => d.size < 100000)
  println(part1.map(_.size).sum)

  val total = 70000000
  val required = 30000000
  val free = total - root.size
  val needed = required - free

  val part2 = findDirs(root, d => d.size >= needed).minBy(_.size)
  println(part2.size)
