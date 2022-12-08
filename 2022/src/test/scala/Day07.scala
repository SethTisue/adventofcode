import collection.mutable.ListBuffer

class Day07 extends munit.FunSuite:

  // data model & parsing: directory tree

  enum Node:
    case Directory(name: String, children: ListBuffer[Node] = ListBuffer.empty)
    case File(name: String, size: Long)
  object Node:
    def fromString(s: String) = s match
      case s"dir $name" => Node.Directory(name)
      case s"$size $name" => Node.File(name, size.toLong)

  def totalSize(e: Node): Long = e match
    case d: Node.Directory => d.children.map(totalSize).sum
    case Node.File(_, size) => size

  def allSubdirs(root: Node.Directory): Iterator[Node.Directory] =
    Iterator(root) ++
      root.children.collect{case d: Node.Directory => d}
        .iterator.flatMap(allSubdirs)
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

  def run(lines: List[Command], dirs: List[Node.Directory]): Unit =
    lines match
      case Nil => // done
      case line :: more =>
        line match
          case Command.Cd("/") =>
            run(more, List(dirs.last))
          case Command.Cd("..") =>
            run(more, dirs.tail)
          case Command.Cd(dest) =>
            val newCwd =
              dirs.head.children.collectFirst{
                case dir @ Node.Directory(`dest`, _) => dir
              }.get
            run(more, newCwd :: dirs)
          case Command.Ls =>
            val (outputLines, more2) = more.span(_.isInstanceOf[Command.Output])
            for Command.Output(s) <- outputLines.map(_.asInstanceOf[Command.Output]) do
              dirs.head.children += Node.fromString(s)
            run(more2, dirs)
          case _: Command.Output =>
            throw new IllegalStateException(line.toString)

  // part 1 code

  def solve1(root: Node.Directory): Long =
    allSubdirs(root)
      .map(totalSize)
      .filter(_ <= 100000L)
      .sum

  // part 2 code

  def solve2(root: Node.Directory): Long =
    val sizeNeeded = totalSize(root) - 40000000L
    allSubdirs(root)
      .map(totalSize)
      .filter(_ >= sizeNeeded)
      .min

  // tests

  def testDay7(name: String, filename: String, solver: Node.Directory => Long, answer: Long) =
    test(s"day 7 $name") {
      val lines = io.Source.fromResource(filename).getLines.map(Command.fromString).toList
      val root: Node.Directory = Node.Directory("/")
      run(lines, List(root))
      assertEquals(solver(root), answer)
    }

  testDay7("part 1 sample", "day07-sample.txt", solve1, 95437L)
  testDay7("part 1",        "day07.txt",        solve1, 1243729L)
  testDay7("part 2 sample", "day07-sample.txt", solve2, 24933642L)
  testDay7("part 2",        "day07.txt",        solve2, 4443914L)

end Day07
