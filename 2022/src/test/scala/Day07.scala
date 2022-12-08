class Day07 extends munit.FunSuite:

  // I/O

  def getInput(name: String): List[Command] =
    io.Source.fromResource(name)
      .getLines
      .map(Command.fromString)
      .toList

  // data model & parsing: directory tree

  import collection.mutable.ListBuffer

  enum Entry:
    case Directory(name: String, entries: ListBuffer[Entry])
    case File(name: String, size: Long)
  object Entry:
    def fromString(s: String) = s match
      case s"dir $name" => Entry.Directory(name, ListBuffer.empty)
      case s"$size $name" => Entry.File(name, size.toLong)
  def root: Entry.Directory =
    Entry.Directory("/", ListBuffer())

  def totalSize(dir: Entry.Directory): Long =
    val files = dir.entries.collect{case f: Entry.File => f}
    val dirs = dir.entries.collect{case d: Entry.Directory => d}
    files.map(_.size).sum + dirs.map(totalSize).sum

  // data model & parsing: commands

  enum Command:
    case Cd(dest: String)
    case Ls
    case Output(s: String)
  object Command:
    def fromString(s: String) = s match
      case "$ ls" => Ls
      case s"$$ cd $dest" => Cd(dest)
      case _ => Output(s)

  // interpreter

  def runCommands(lines: List[Command], dirs: List[Entry.Directory]): Unit =
    lines match
      case Nil =>
      case line :: more =>
        line match
          case Command.Cd("/") =>
            runCommands(more, List(dirs.last))
          case Command.Cd("..") =>
            runCommands(more, dirs.tail)
          case Command.Cd(dest) =>
            val newCwd = dirs.head.entries.collectFirst{
              case dir @ Entry.Directory(`dest`, _) => dir
            }.get
            runCommands(more, newCwd :: dirs)
          case Command.Ls =>
            val (outputLines, more2) = more.span(_.isInstanceOf[Command.Output])
            for Command.Output(s) <- outputLines.map(_.asInstanceOf[Command.Output])
            do dirs.head.entries += Entry.fromString(s)
            runCommands(more2, dirs)
          case _ =>
            throw new IllegalStateException(line.toString)

  // part 1 code

  def solve1(root: Entry.Directory): Long =
    var sum = 0L
    def recurse(dir: Entry.Directory): Unit =
      val size = totalSize(dir)
      if size <= 100000L then
        sum += size
      dir.entries.collect{case d: Entry.Directory => d}.foreach(recurse)
    recurse(root)
    sum

  // part 2 code

  def solve2(root: Entry.Directory): Long =
    val sizeNeeded = totalSize(root) - 40000000L
    def allSubdirs(root: Entry.Directory): Iterator[Entry.Directory] =
      Iterator(root) ++ root.entries.collect{case d: Entry.Directory => d}.iterator.flatMap(allSubdirs)
    allSubdirs(root).map(totalSize).filter(_ >= sizeNeeded).min

  // part 1 tests

  test("day 7 part 1 sample") {
    val lines = getInput("day07-sample.txt")
    val myRoot = root
    runCommands(lines, List(myRoot))
    assertEquals(95437L, solve1(myRoot))
  }

  test("day 7 part 1") {
    val lines = getInput("day07.txt")
    val myRoot = root
    runCommands(lines, List(myRoot))
    assertEquals(1243729L, solve1(myRoot))
  }

  // part 2 tests

  test("day 7 part 2 sample") {
    val lines = getInput("day07-sample.txt")
    val myRoot = root
    runCommands(lines, List(myRoot))
    assertEquals(24933642L, solve2(myRoot))
  }

  test("day 7 part 2") {
    val lines = getInput("day07.txt")
    val myRoot = root
    runCommands(lines, List(myRoot))
    assertEquals(4443914L, solve2(myRoot))
  }

end Day07
