class Day08 extends munit.FunSuite:

  /// core logic

  type Position = (Int, Int)  // row, column

  case class Antenna(pos: Position, frequency: Char)

  case class Board(size: Int, antennae: Set[Antenna]):
    def inBounds(pos: Position) =
      pos._1 >= 0 && pos._1 < size &&
        pos._2 >= 0 && pos._2 < size
    def pairs: Set[(Antenna, Antenna)] =
      for a1 <- antennae
          a2 <- antennae
          if a1.frequency == a2.frequency && a1 != a2
      yield (a1, a2)
    def antinodes(a1: Antenna, a2: Antenna): LazyList[Position] =
      LazyList.iterate((a2.pos._1, a2.pos._2)): pos =>
        (pos._1 + a2.pos._1 - a1.pos._1, pos._2 + a2.pos._2 - a1.pos._2)
      .takeWhile(inBounds)

  /// reading & parsing

  def getInput(name: String): Board =
    val lines =
      io.Source.fromResource(name)
        .getLines
        .toIndexedSeq
    Board(lines.size,
      for row <- (0 until lines.size).toSet
          column <- 0 until lines.size
          freq = lines(row).charAt(column)
          if freq != '.'
      yield Antenna((row, column), freq))

  /// part 1

  def solve(name: String, part1: Boolean): Int =
    val board = getInput(name)
    def cull(antinodes: LazyList[Position]) =
      if part1
      then antinodes.drop(1).take(1)
      else antinodes
    val results =
      for (a1, a2) <- board.pairs
          anti1 = cull(board.antinodes(a1, a2))
          anti2 = cull(board.antinodes(a2, a1))
      yield (anti1 ++ anti2)
    results.flatten.size

  def part1(name: String): Int =
    solve(name, part1 = true)

  test("part 1 sample"):
    assertEquals(part1("day08-sample.txt"), 14)
  test("part 1"):
    assertEquals(part1("day08.txt"), 222)

  /// part 2

  def part2(name: String): Int =
    solve(name, part1 = false)

  test("part 2 sample"):
    assertEquals(part2("day08-sample.txt"), 34)
  test("part 2"):
    assertEquals(part2("day08.txt"), 884)

end Day08
