class Day08 extends munit.FunSuite:

  /// core logic

  type Position = (Int, Int)

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
      LazyList.iterate((a2.pos._1, a2.pos._2)){case (r, c) =>
        (r + a2.pos._1 - a1.pos._1, c + a2.pos._2 - a1.pos._2)}

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

  def part1(name: String): Int =
    val board = getInput(name)
    val as =
      for (a1, a2) <- board.pairs
          anti1 = board.antinodes(a1, a2).drop(1).head
          anti2 = board.antinodes(a2, a1).drop(1).head
      yield Set(anti1, anti2).filter(board.inBounds)
    as.flatten.size

  test("part 1 sample"):
    assertEquals(part1("day08-sample.txt"), 14)
  test("part 1"):
    assertEquals(part1("day08.txt"), 222)

  /// part 2

  def part2(name: String): Int =
    val board = getInput(name)
    val as =
      for (a1, a2) <- board.pairs
          antis1 = board.antinodes(a1, a2).takeWhile(board.inBounds)
          antis2 = board.antinodes(a2, a1).takeWhile(board.inBounds)
      yield (antis1.toSet ++ antis2.toSet)
    as.flatten.size

  test("part 2 sample"):
    assertEquals(part2("day08-sample.txt"), 34)
  test("part 2"):
    assertEquals(part2("day08.txt"), 884)

end Day08
