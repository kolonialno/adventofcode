import scala.io.Source
import scala.collection.mutable.Stack

case class Dir(name: String, subdirs: List[Dir], files: List[File]) {
  def size: Int = subdirs.map(_.size).sum + files.map(_.size).sum
}
case class File(name: String, size: Int)

def buildTree(dirName: String, data: Stack[String]): Dir =
  var dir = Dir(dirName, Nil, Nil)
  while (data.nonEmpty)
    val cmd = data.pop
    if cmd == "$ cd .." then
      return dir
    else if cmd.startsWith("$ cd ") then
      val subdirName = cmd.drop(5)
      val subdir = buildTree(subdirName, data)
      dir = dir.copy(subdirs = subdir :: dir.subdirs)
    else if cmd != "$ ls" && !cmd.startsWith("dir ") then
      val Array(size, name) = cmd.split(" ")
      val file = File(name, size.toInt)
      dir = dir.copy(files = file :: dir.files)
  return dir

def findDirs(dir: Dir, cond: Dir => Boolean): List[Dir] =
  val children = dir.subdirs.flatMap(findDirs(_, cond))
  if cond(dir) then dir :: children else children

@main def main =
  val data = Source.fromFile("input.txt").getLines.toList
  val s = Stack[String]()
  s.pushAll(data.reverse)
  val first = s.pop() // "$ cd /"
  val root = buildTree("/", s)

  // println(root)
  val part1 = findDirs(root, d => d.size < 100000)
  // println(part1)
  println(part1.map(_.size).sum)

  val total = 70000000
  val required = 30000000
  val free = total - root.size
  val needed = required - free

  val part2 = findDirs(root, d => d.size >= needed).minBy(_.size)
  println(part2.size)
