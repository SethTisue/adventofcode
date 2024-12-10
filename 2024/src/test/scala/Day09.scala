final class Day09 extends munit.FunSuite:

  // shared logic

  enum Segment:
    def length: Int
    case File(id: Int, length: Int)
    case Empty(length: Int)

  import Segment.{File, Empty}

  def expand(disk: Vector[Segment]): Vector[Option[Int]] =
    disk.flatMap:
      case File(id, length) => Vector.fill(length)(Some(id))
      case Empty(length) => Vector.fill(length)(None)

  def checksum(disk: Vector[Option[Int]]): Long =
    val items =
      for
        case (Some(id), i) <- disk.zipWithIndex
      yield
        id.toLong * i
    items.sum

  /// reading & parsing

  def getInput(name: String): Vector[Segment] =
    io.Source.fromResource(name)
      .getLines
      .next()
      .grouped(2).zipWithIndex.flatMap: (pair, i) =>
        def file = File(i, pair(0) - '0')
        def empty = Empty(pair(1) - '0')
        pair.size match
          case 2 => Vector(file, empty)
          case 1 => Vector(file)
      .toVector

  /// part 1

  @annotation.tailrec
  def defrag1(disk: Vector[Option[Int]]): Vector[Option[Int]] =
    val lastBlock = disk.lastIndexWhere(_.isDefined)
    val firstEmpty = disk.indexOf(None)
    if firstEmpty >= lastBlock
    then disk
    else
      defrag1(
        disk.updated(firstEmpty, disk(lastBlock))
          .updated(lastBlock, None))

  def part1(name: String): Long =
    checksum(defrag1(expand(getInput(name))))

  test("part 1 sample 1"):
    assertEquals(part1("day09-sample1.txt"), 60L)
  test("part 1 sample 2"):
    assertEquals(part1("day09-sample2.txt"), 1928L)
  test("part 1"):
    assertEquals(part1("day09.txt"), 6283170117911L)

  /// part 2

  @annotation.tailrec
  final def defrag2(disk: Vector[Segment], result: Vector[Segment] = Vector.empty): Vector[Segment] =
    if disk.isEmpty
    then
      result
    else disk.last match
      case empty: Empty =>
        defrag2(disk.init, empty +: result)
      case file: File =>
        val (before, after) = disk.span:
          case Empty(length) if length >= file.length =>
            false
          case _ =>
            true
        if after.isEmpty
        then defrag2(disk.init, file +: result)
        else
          val replacement = Vector(file, Empty(after.head.length - file.length))
          defrag2(
            before ++ replacement ++ after.tail.init,
            Empty(file.length) +: result)

  def part2(name: String): Long =
    checksum(expand(defrag2(getInput(name))))

  test("part 2 sample 2"):
    assertEquals(part2("day09-sample2.txt"), 2858L)
  test("part 2"):
    assertEquals(part2("day09.txt"), 6307653242596L)

end Day09
