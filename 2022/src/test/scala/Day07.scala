class Day07 extends munit.FunSuite:

  def getInput(name: String): List[Command] =
    io.Source.fromResource(name)
      .getLines
      .map(Command.fromString)
      .toList

  import collection.mutable.ListBuffer

  enum Entry:
    case Directory(name: String, entries: ListBuffer[Entry])
    case File(name: String, size: Long)

  def root: Entry.Directory =
    Entry.Directory("/", ListBuffer())

  enum Command:
    case Cd(dest: String)
    case Ls
    case Output(s: String)
  object Command:
    def fromString(s: String) = s match
      case "$ ls" => Ls
      case s"$$ cd $dest" => Cd (dest)
      case _ => Output(s)

  def solve(lines: List[Command], tree: Entry.Directory = root): Long =
    lines match
      case Nil => 0
      case line :: more => 0

  // part 1 tests

  test("day 7 part 1 sample") {
    val lines = getInput("day07-sample.txt")
    assertEquals(95437L, solve(lines))
  }

  test("day 7 part 1") {
    val lines = getInput("day07.txt")
    assertEquals(0L, solve(lines))
  }

end Day07
